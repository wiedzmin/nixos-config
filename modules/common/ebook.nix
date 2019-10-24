{ config, lib, pkgs, ... }:
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
    };
  };

  config = mkMerge [
    (mkIf cfg.readers.enable {
      fileSystems."${config.services.syncthing.dataDir}/bookshelf" = {
        device = "/home/${config.attributes.mainUser.name}/bookshelf";
        options = [ "bind" ];
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          calibre
          djview
          djvulibre
        ];
        programs.zathura = {
          enable = true;
          options = {
            pages-per-row = 1;
          };
        };
      };
    })
    (mkIf cfg.processors.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          enca
          img2pdf
          pandoc
          pdfcpu
          pdftk
        ] ++ lib.optionals (config.attributes.staging.enable) [
          pdfarranger
          wpWorking.python3Packages.weasyprint
        ];
      };
    })
  ];
}
