{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.content.ebooks;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    content.ebooks = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ebooks infrastructure.";
      };
      roots = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Paths to search ebooks under.";
      };
      extensions.primary = mkOption {
        type = types.listOf types.str;
        default = [ "pdf" "djvu" "epub" ];
        description = "Main ebook file extensions to consider.";
      };
      extensions.secondary = mkOption {
        type = types.listOf types.str;
        default = [ "mobi" "fb2" ];
        description = "Auxillary ebook file extensions, mostly for timetracking at the moment.";
      };
      searchCommand = mkOption {
        type = types.str;
        default = "${pkgs.fd}/bin/fd --full-path --absolute-path ${
            lib.concatStringsSep " " (lib.forEach cfg.extensions.primary (ext: "-e ${ext}"))
          }";
        visible = false;
        internal = true;
        description = "Shell command to use for collecting ebooks' paths.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      fileSystems."${config.services.syncthing.dataDir}/bookshelf" = {
        device = homePrefix "bookshelf";
        options = [ "bind" ];
      };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set content/ebook_roots ${
          lib.strings.escapeNixString (builtins.toJSON (localEbooks config.custom.navigation.bookmarks.entries))
        }
      '';
      nixpkgs.config.packageOverrides = _: rec {
        bookshelf = mkPythonScriptWithDeps "bookshelf" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis zathura ])
          (readSubstituted ../../subst.nix ./scripts/bookshelf.py);
        update-bookshelf =
          mkPythonScriptWithDeps "update-bookshelf" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis ])
          (readSubstituted ../../subst.nix ./scripts/update-bookshelf.py);
      };
      systemd.user.services."update-ebooks" = {
        description = "Update bookshelf contents";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.update-bookshelf}/bin/update-bookshelf";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."update-ebooks" = renderTimer "Update ebooks entries" "1h" "1h" "";
      custom.pim.timeTracking.rules = ''
        current window $title =~ m!.*papers/.*! ==> tag ebooks:papers,
        ${
          concatStringsSep ''
            ,
          '' (lib.forEach (with cfg.extensions; primary ++ secondary)
            (ext: "current window $title =~ /.*${ext}.*/ ==> tag read:${ext}"))
        },
      '';
      home-manager.users.${user} = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.ebook "org.pwmt.zathura.desktop";
        home.packages = with pkgs; [ calibre djview djvulibre ];
        programs.zathura = {
          enable = true;
          options = {
            pages-per-row = 1;
            selection-clipboard = "clipboard";
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "b" ];
        cmd = "${pkgs.bookshelf}/bin/bookshelf";
        mode = "select";
      }];
    })
    (mkIf (config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ bookshelf update-bookshelf ]; };
    })
  ];
}
