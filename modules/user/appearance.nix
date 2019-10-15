{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.appearance;
  rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
    ${pkgs.feh}/bin/feh --bg-fill ${cfg.wallpaper.root}/${cfg.wallpaper.current}
  '';
in {
  options = {
    appearance = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable appearance customizations.";
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
      wallpaper.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable wallpapers functionality.";
      };
      wallpaper.root = mkOption {
        type = types.str;
        default = "";
        description = "Wallpapers root directory.";
      };
      wallpaper.current = mkOption {
        type = types.str;
        default = "";
        description = "Current wallpaper.";
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
    (mkIf (cfg.enable && cfg.wallpaper.enable) {
      assertions = [{
        assertion = cfg.wallpaper.root != "" && cfg.wallpaper.current != "";
        message = "appearance: must provide wallpapers path and image to use.";
      }];

      environment.systemPackages = with pkgs; [
        rescale-wallpaper
      ];

      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.autorandr.hooks = {
          postswitch = { "rescale-wallpaper" = "${rescale-wallpaper}/bin/rescale-wallpaper"; };
        };
      };
    })
  ];
}
