{ config, lib, pkgs, ... }:

with lib;
let cfg = config.themes.fonts.fira-code;
in {
  options.themes.fonts.fira-code = { enable = mkEnableOption "fira-code"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [ fira-code ];
      fontconfig = { defaultFonts = { monospace = [ "Fira Code" ]; }; };
    };
    wmCommon.fonts.default = "pango:Fira Code Bold 8";
    wmCommon.fonts.dmenu = "Fira Code:bold:pixelsize=12";
    wmCommon.fonts.statusbar = "pango:Fira Code Bold 8";
    ide.emacs.fontSpec = "Fira Code:size=12";
    home-manager.users."${config.attributes.mainUser.name}" = {
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
      programs.zathura.options.font = "Fira Code Bold 8";
      services.dunst.settings.global.font = "Fira Code Bold 8";
      xresources.properties = {
        "Xmessage*faceName" = "Fira Code";
        "Xmessage*faceSize" = "12";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "Fira Code:weight=Bold:size=12";
      };
    };
  };
}
