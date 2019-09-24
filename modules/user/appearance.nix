{ config, lib, pkgs, ... }:
with lib;

let cfg = config.appearance;
in {
  options = {
    appearance = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable appearance customizations.";
      };
      fonts.list = mkOption {
        type = types.listOf types.path;
        default = [ ];
        example = [ pkgs.hack-font ];
        description = "Fonts packages.";
      };
      fonts.antialias = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable fonts antialiasing.";
      };
      fonts.dpi = mkOption {
        type = types.int;
        default = 115;
        description = "Font DPI.";
      };
      fonts.default.monospace = mkOption {
        type = types.str;
        default = "Iosevka";
        description = "Default monospace font family.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      fonts = {
        fonts = cfg.fonts.list;
        fontconfig = {
          enable = true;
          antialias = cfg.fonts.antialias;
          dpi = cfg.fonts.dpi;
          defaultFonts = { monospace = [ cfg.fonts.default.monospace ]; };
        };
        enableFontDir = true;
        enableGhostscriptFonts = true;
      };
    })
  ];
}
