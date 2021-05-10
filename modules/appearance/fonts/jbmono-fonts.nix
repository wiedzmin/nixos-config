{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.appearance.fonts.jetbrains-mono;
  user = config.attributes.mainUser.name;
  beautify = config.appearance.fonts.beautify;
in {
  options.appearance.fonts.jetbrains-mono = { enable = mkEnableOption "jetbrains-mono"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [ jetbrains-mono ];
      fontconfig = { defaultFonts = { monospace = [ "JetBrains Mono" ]; }; };
    };
    wmCommon.fonts.default = "pango:${if beautify then "JetBrainsMono Nerd Font " else "JetBrains Mono "}Bold 8";
    wmCommon.fonts.dmenu = "JetBrains Mono:bold:size=8";
    wmCommon.fonts.statusbar = "pango:${if beautify then "JetBrainsMono Nerd Font " else "JetBrains Mono "}Bold 8";
    environment.sessionVariables.TB_SELECTOR_FONT = [ "JetBrains Mono:bold:size=8" ];
    home-manager.users.${user} = {
      programs.zsh.sessionVariables = {
        TB_SELECTOR_FONT = "JetBrains Mono:bold:size=8";
      };
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
      programs.rofi.font = "JetBrains Mono Bold 11";
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
