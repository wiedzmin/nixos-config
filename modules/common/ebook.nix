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
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          calibre
          djview
          djvulibre
        ];
      };
    })
    (mkIf cfg.processors.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          # wpWorking.python3Packages.weasyprint
          enca
          img2pdf
          pandoc
          pdfcpu
          pdftk
        ] ++ lib.optionals (config.attributes.staging.enable) [
          pdfarranger
        ];
      };
    })
  ];
}
