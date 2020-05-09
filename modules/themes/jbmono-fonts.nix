{ config, lib, pkgs, ... }:

with lib;
let cfg = config.themes.fonts.jetbrains-mono;
in {
  options.themes.fonts.jetbrains-mono = { enable = mkEnableOption "jetbrains-mono"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [
        jetbrains-mono

        config.attributes.fonts.basic.package
      ];
    };
    wm.xmonad.fonts.default = "xft:JetBrains Mono:weight=Bold:size=10";
    wm.xmonad.fonts.xmobar = config.custom.appearance.fonts.basic.xft;
    wm.xmonad.fonts.dmenu = "xft:JetBrains Mono:size=8";
    home-manager.users."${config.attributes.mainUser.name}" = {
      gtk.font = lib.optionalAttrs (config.custom.appearance.gtk.enable) {
        package = config.custom.appearance.fonts.basic.package;
        name = config.custom.appearance.fonts.basic.raw;
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
      programs.zathura.options.font = "JetBrains Mono Bold 10";
      services.dunst.settings.global.font = "JetBrains Mono Bold 8";
      xresources.properties = {
        "Emacs*XlwMenu.font" = "JetBrains Mono:weight=Bold:size=12";
        "Emacs.Font" = "JetBrains Mono:weight=Bold:size=12";
        "Emacs.dialog*.font" = "JetBrains Mono:weight=Bold:size=12";

        "Xmessage*faceName" = "JetBrains Mono";
        "Xmessage*faceSize" = "12";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "JetBrains Mono:weight=Bold:size=12";
      };
    };
  };
}
