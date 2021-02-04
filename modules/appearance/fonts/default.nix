{ config, inputs, lib, pkgs, ... }:
with lib;

let
  cfg = config.appearance.fonts;
  nixpkgs-prev = import inputs.nixpkgs-25_01_21 ({
    config = config.nixpkgs.config;
    system = "x86_64-linux";
  });
in {
  imports = [
    ./fira-code-fonts.nix
    ./hack-fonts.nix
    ./iosevka-fonts.nix
    ./jbmono-fonts.nix
    ./scp-fonts.nix
  ];

  options = {
    appearance.fonts = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable fonts customization.";
      };
      beautify = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable iconified patched fonts.";
      };
      antialias = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable antialiasing.";
      };
      dpi = mkOption {
        type = types.int;
        default = 115;
        description = "Font DPI.";
      };
      console = mkOption {
        type = types.str;
        default = "";
        description = "Font for console.";
      };
      locale = mkOption {
        type = types.str;
        default = "ru_RU.UTF-8";
        description = "Locale name.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      fonts = {
        fontconfig = {
          enable = true;
          antialias = cfg.antialias;
          dpi = cfg.dpi;
        };
        fontDir.enable = true;
        enableGhostscriptFonts = true;
        enableDefaultFonts = true;
      } // lib.optionalAttrs (cfg.beautify) { fonts = with pkgs; [ nixpkgs-prev.nerdfonts ]; };
      console = {
        font = cfg.console;
        useXkbConfig = true;
      };
      i18n = { defaultLocale = cfg.locale; };
    })
  ];
}
