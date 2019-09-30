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
      fonts.console = mkOption {
        type = types.str;
        default = "";
        description = "Font for console.";
      };
      fonts.locale = mkOption {
        type = types.str;
        default = "ru_RU.UTF-8";
        description = "Locale name.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = cfg.fonts.console != "";
        message = "appearance: must provide console font name.";
      }];

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
      i18n = {
        consoleFont = cfg.fonts.console;
        defaultLocale = cfg.fonts.locale;
        consoleUseXkbConfig = true;
        inputMethod = {
          enabled = "ibus";
          ibus.engines = with pkgs.ibus-engines; [
            table
            table-others # for LaTeX input
            m17n
          ];
        };
      };
    })
  ];
}
