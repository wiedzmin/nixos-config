{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.themes.hackFonts;
in {
  options.themes.hackFonts = {
    enable = mkEnableOption "hackFonts";
  };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [
        hack-font
      ];
    };
    home-manager.users."${config.attributes.mainUser.name}" = {
      gtk.font = {
        package = pkgs.hack-font;
        name = "Hack Bold 8";
      };
      programs.alacritty.settings.font = {
        normal = {
          family = "Hack";
          Style = "Bold";
        };
        bold = {
          family = "Hack";
          style = "Bold";
        };
        italic = {
          family = "Hack";
          style = "Italic";
        };
        size = 11.0;
      };
      programs.rofi.font = "Hack Bold 10";
      programs.zathura.options.font = "Hack Bold 10";
      services.dunst.settings.global.font = "Hack Bold 10";
      xresources.properties = {
        "Emacs*XlwMenu.font" = "Hack:weight=Bold:size=14";
        "Emacs.Font" = "Hack:weight=Bold:size=14";
        "Emacs.dialog*.font" = "Hack:weight=Bold:size=14";

        "Xmessage*faceName" = "Hack";
        "Xmessage*faceSize" = "16";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "Hack:weight=Bold:size=16";
      };
    };
  };
}
