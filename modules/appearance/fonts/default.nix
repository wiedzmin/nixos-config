{ config, inputs, lib, pkgs, ... }:
with lib;

# TODO: review https://devfonts.gafi.dev/ and https://coding-fonts.css-tricks.com/ lists

let
  cfg = config.appearance.fonts;
  user = config.attributes.mainUser.name;
in
{
  imports = [ ./fira-code-fonts.nix ./hack-fonts.nix ./iosevka-fonts.nix ./jbmono-fonts.nix ./scp-fonts.nix ];

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
          inherit (cfg) antialias;
        };
        fontDir.enable = true;
        enableGhostscriptFonts = true;
        enableDefaultFonts = true;
      } // lib.optionalAttrs cfg.beautify { fonts = with pkgs; [ nerdfonts ]; };
      console = {
        font = cfg.console;
        useXkbConfig = true;
      };
      i18n = { defaultLocale = cfg.locale; };
      home-manager.users."${user}" = { home.packages = with pkgs; [ cicero-tui fontfor ]; };
    })
  ];
}
