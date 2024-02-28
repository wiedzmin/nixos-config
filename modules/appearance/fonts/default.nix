{ config, inputs, lib, pkgs, ... }:
with lib;

let
  cfg = config.appearance.fonts;
  user = config.attributes.mainUser.name;
  nixpkgs-last-unbroken = import inputs.nixpkgs-last-unbroken {
    config = config.nixpkgs.config // {
      allowUnfree = true;
      permittedInsecurePackages = config.ext.nix.core.permittedInsecurePackages;
    };
    localSystem = { system = "x86_64-linux"; };
  };
in
{
  imports = [ ./agave-fonts.nix ./fira-code-fonts.nix ./hack-fonts.nix ./iosevka-fonts.nix ./jbmono-fonts.nix ./scp-fonts.nix ];

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
        packages = with pkgs; [
          (nerdfonts.override
            {
              fonts = [
                "FiraCode"
                "Hack"
                "Iosevka"
                "JetBrainsMono"
                "SourceCodePro"
              ];
            })
        ];
      };
      console = {
        font = cfg.console;
        useXkbConfig = true;
      };
      i18n = { defaultLocale = cfg.locale; };
      home-manager.users."${user}" = { home.packages = with pkgs; [ cicero-tui nixpkgs-last-unbroken.fontfor ]; };
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
