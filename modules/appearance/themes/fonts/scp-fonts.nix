{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.themes.fonts.source-code-pro;
  user = config.attributes.mainUser.name;
  beautify = config.custom.appearance.fonts.beautify;
in {
  options.themes.fonts.source-code-pro = { enable = mkEnableOption "source-code-pro"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [ source-code-pro ];
      fontconfig = { defaultFonts = { monospace = [ "Source Code Pro" ]; }; };
    };
    wmCommon.fonts.default = "pango:${if beautify then "SauceCodePro Nerd Font " else "Source Code Pro "}Bold 9";
    wmCommon.fonts.dmenu = "Source Code Pro:bold:size=9";
    wmCommon.fonts.statusbar = "pango:${if beautify then "SauceCodePro Nerd Font " else "Source Code Pro "}Bold 9";
    home-manager.users.${user} = {
      gtk.font = lib.optionalAttrs (config.custom.appearance.gtk.enable) {
        package = pkgs.source-code-pro;
        name = "Source Code Pro Bold 8";
      };
      programs.alacritty.settings.font = {
        normal = {
          family = "Source Code Pro";
          Style = "Bold";
        };
        bold = {
          family = "Source Code Pro";
          style = "Bold";
        };
        italic = {
          family = "Source Code Pro";
          style = "Italic";
        };
        size = 11.0;
      };
      programs.zathura.options.font = "Source Code Pro Bold 10";
      services.dunst.settings.global.font = "Source Code Pro Bold 10";
      xresources.properties = {
        "Emacs.Font" = "Source Code Pro:weight=Bold:size=12";

        "Xmessage*faceName" = "Source Code Pro";
        "Xmessage*faceSize" = "12";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "Source Code Pro:weight=Bold:size=12";
      };
    };
  };
}
