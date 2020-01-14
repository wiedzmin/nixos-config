{ config, lib, pkgs, ... }:

with lib;
let
    cfg = config.themes.fonts.fira-code;
in {
  options.themes.fonts.fira-code = {
    enable = mkEnableOption "fira-code";
  };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [
        fira-code
      ];
    };
    wm.xmonad.font = "xft:Fira Code:style=Bold:pixelsize=10";
    attributes.fonts.xmobar = "xft:Fira Code:weight=Bold:size=8";
    attributes.fonts.dmenu = "xft:Fira Code:style=Bold:pixelsize=12";
    attributes.fonts.xmonadDefault = "xft:Fira Code:weight=Bold:size=10";
    home-manager.users."${config.attributes.mainUser.name}" = {
      # skipping gtk fontification, it breaks things for some reason.
      programs.alacritty.settings.font = {
        normal = {
          family = "Fira Code";
          Style = "Bold";
        };
        bold = {
          family = "Fira Code";
          style = "Bold";
        };
        italic = {
          family = "Fira Code";
          style = "Italic";
        };
        size = 11.0;
      };
      programs.zathura.options.font = "Fira Code Bold 10";
      services.dunst.settings.global.font = "Fira Code Bold 10";
      xresources.properties = {
        "Emacs*XlwMenu.font" = "Fira Code:weight=Bold:size=12";
        "Emacs.Font" = "Fira Code:weight=Bold:size=12";
        "Emacs.dialog*.font" = "Fira Code:weight=Bold:size=12";

        "Xmessage*faceName" = "Fira Code";
        "Xmessage*faceSize" = "12";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "Fira Code:weight=Bold:size=12";
      };
    };
  };
}
