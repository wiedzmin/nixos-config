{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.tools.ebooks;
in {
  options = {
    tools.ebooks = {
      readers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ebook readers for various formats.";
      };
      readers.roots = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Paths to search ebooks under.";
      };
      readers.extensions = mkOption {
        type = types.listOf types.str;
        default = [ "pdf" "djvu" "epub" ];
        description = "Ebook file extensions to consider.";
      };
      readers.booksSearchCommand = mkOption {
        type = types.str;
        default = "${pkgs.fd}/bin/fd --full-path --absolute-path ${
            lib.concatStringsSep " " (lib.forEach cfg.readers.extensions (ext: "-e ${ext}"))
          }";
        visible = false;
        internal = true;
        description = "Shell command to use for collecting ebooks' paths.";
      };
      processors.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ebooks processors (mostly pdf-centric).";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
      staging.packages = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "List of staging packages.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.readers.enable {
      fileSystems."${config.services.syncthing.dataDir}/bookshelf" = {
        device = "/home/${config.attributes.mainUser.name}/bookshelf";
        options = [ "bind" ];
      };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set content/ebook_roots ${
          lib.strings.escapeNixString (builtins.toJSON cfg.readers.roots)
        }
      '';
      nixpkgs.config.packageOverrides = _: rec {
        bookshelf = writePythonScriptWithPythonPackages "bookshelf" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.redis
          pkgs.zathura
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./bookshelf.py; })));
        update-bookshelf = writePythonScriptWithPythonPackages "update-bookshelf" [ pkgs.python3Packages.redis ]
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./update-bookshelf.py; })));
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
      home-manager.users."${config.attributes.mainUser.name}" = {
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
    (mkIf cfg.processors.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ enca img2pdf pandoc pdfcpu pdftk ];
      };
    })
    (mkIf (cfg.xmonad.enable && cfg.readers.enable) {
      wm.xmonad.keybindings = { "M-r b" = ''spawn "${pkgs.bookshelf}/bin/bookshelf"''; };
    })
    (mkIf (cfg.staging.packages != [ ]) {
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = cfg.staging.packages; };
    })
  ];
}
