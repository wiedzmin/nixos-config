{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.themes.fonts.jetbrains-mono;
  user = config.attributes.mainUser.name;
in {
  options.themes.fonts.jetbrains-mono = { enable = mkEnableOption "jetbrains-mono"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [ jetbrains-mono ];
      fontconfig = { defaultFonts = { monospace = [ "JetBrains Mono" ]; }; };
    };
    wmCommon.fonts.default = "pango:JetBrains Mono Bold 8";
    wmCommon.fonts.dmenu = "JetBrains Mono:bold:size=8";
    wmCommon.fonts.statusbar = "pango:JetBrains Mono Bold 8";
    home-manager.users.${user} = {
      programs.alacritty.settings.font = {
        normal = {
          family = "JetBrains Mono";
          Style = "Bold";
        };
        bold = {
          family = "JetBrains Mono";
          style = "Bold";
        };
        italic = {
          family = "JetBrains Mono";
          style = "Italic";
        };
        size = 11.0;
      };
      programs.zathura.options.font = "JetBrains Mono Bold 9";
      services.dunst.settings.global.font = "JetBrains Mono Bold 8";
      xresources.properties = {
        "Emacs.Font" = "JetBrains Mono:weight=Bold:size=12";

        "Xmessage*faceName" = "JetBrains Mono";
        "Xmessage*faceSize" = "12";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "JetBrains Mono:weight=Bold:size=12";
      };
    };
  };
}
