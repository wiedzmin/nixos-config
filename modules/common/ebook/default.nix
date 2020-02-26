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
      staging.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable staging settings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.readers.enable {
      fileSystems."${config.services.syncthing.dataDir}/bookshelf" = {
        device = "/home/${config.attributes.mainUser.name}/bookshelf";
        options = [ "bind" ];
      };
      nixpkgs.config.packageOverrides = _: rec {
        bookshelf = writePythonScriptWithPythonPackages "bookshelf" [ pkgs.python3Packages.dmenu-python ]
          (builtins.readFile
            (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./bookshelf.py; })));
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.file = {
          ".local/share/applications/defaults.list" = {
            text = ''
              [Default Applications]
              application/pdf=zathura.desktop
            '';
          };
        };
        home.packages = with pkgs; [ calibre djview djvulibre ] ++ lib.optionals (cfg.staging.enable) [ epr ];
        programs.zathura = {
          enable = true;
          options = { pages-per-row = 1; };
        };
      };
    })
    (mkIf cfg.processors.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs;
          [ enca img2pdf pandoc pdfcpu pdftk ]
          ++ lib.optionals (cfg.staging.enable) [ pdfarranger wpWorking.python3Packages.weasyprint ];
      };
    })
    (mkIf (cfg.xmonad.enable && cfg.readers.enable) {
      wm.xmonad.keybindings = { "M-S-b" = ''spawn "${pkgs.bookshelf}/bin/bookshelf"''; };
    })
  ];
}
