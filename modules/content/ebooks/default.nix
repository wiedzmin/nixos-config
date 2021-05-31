{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.content.ebooks;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    content.ebooks = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ebooks infrastructure.";
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
      nixpkgs.config.packageOverrides = _: rec {
        bookshelf = mkPythonScriptWithDeps "bookshelf" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis zathura ])
          (builtins.readFile ./scripts/bookshelf.py);
      };
      systemd.user.services = builtins.listToAttrs (forEach (localEbooks config.navigation.bookmarks.entries) (root:
        let
          token = concatStringsSep "-" (takeLast 2 (splitString "/" root));
        in
        {
          name = "update-ebooks-${token}";
          value = {
            description = "Update ${token} contents";
            after = [ "graphical-session-pre.target" ];
            partOf = [ "graphical-session.target" ];
            wantedBy = [ "graphical-session.target" ];
            path = [ pkgs.bash ];
            serviceConfig = {
              Type = "simple";
              WorkingDirectory = root;
              ExecStart = "${pkgs.watchexec}/bin/watchexec -r --exts ${
                concatStringsSep "," cfg.extensions.primary
              } -- ${nurpkgs.toolbox}/bin/collect --root ${root} --exts ${
                concatStringsSep "," cfg.extensions.primary
              } --key content/${token}/ebooks";
              StandardOutput = "journal";
              StandardError = "journal";
            };
          };
        }));
      pim.timetracking.rules = mkArbttTitleRule [ ".*papers/.*" ] "ebooks:papers" + "\n"
        + mkArbttPrefixedTitlesRule (with cfg.extensions; primary ++ secondary) "read:";
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
        programs.zsh.sessionVariables = {
          TB_EBOOKS_READER_COMMAND = config.attributes.ebookreader.default.cmd;
        };
      };
      attributes.ebookreader.default.cmd = "${pkgs.zathura}/bin/zathura";
      attributes.ebookreader.default.windowClass = [ "Zathura" ];
      environment.sessionVariables.TB_EBOOKS_READER_COMMAND = [ config.attributes.ebookreader.default.cmd ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "b" ];
        cmd = "${pkgs.bookshelf}/bin/bookshelf";
        mode = "select";
      }];
    })
    (mkIf (config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ bookshelf ]; };
    })
  ];
}
