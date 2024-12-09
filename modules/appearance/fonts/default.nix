{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.appearance.fonts;
  user = config.attributes.mainUser.name;
in
{
  imports = [
    ./agave-fonts.nix
    ./cascadia-fonts.nix
    ./fira-code-fonts.nix
    ./geist-fonts.nix
    ./hack-fonts.nix
    ./inconsolata-fonts.nix
    ./iosevka-fonts.nix
    ./jbmono-fonts.nix
    ./martian-fonts.nix
    ./roboto-fonts.nix
    ./scp-fonts.nix
    ./victor-fonts.nix
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
        enableDefaultPackages = true;
      } // lib.optionalAttrs cfg.beautify {
        packages = with pkgs.nerd-fonts; [
          fira-code
          hack
          iosevka
          jetbrains-mono
          sauce-code-pro
        ];
      };
      console = {
        font = cfg.console;
        useXkbConfig = true;
      };
      i18n = { defaultLocale = cfg.locale; };
      home-manager.users."${user}" = { home.packages = with pkgs; [ cicero-tui fontfor ]; };
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "devfonts" = {
          desc = "JS fonts theming and comparison";
          remote.url = "https://devfonts.gafi.dev/";
        };
        "coding-fonts-css-tricks" = {
          desc = "CSS fonts tricks";
          remote.url = "https://coding-fonts.css-tricks.com/";
        };
      };
    })
  ];
}
