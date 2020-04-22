{ config, lib, pkgs, ... }:

with lib;
let cfg = config.themes.fonts.iosevka;
in {
  options.themes.fonts.iosevka = { enable = mkEnableOption "iosevka"; };

  config = mkIf cfg.enable {
    fonts = { fonts = with pkgs; [ iosevka ]; };
    wm.xmonad.font = "xft:Iosevka:style=Bold:pixelsize=10";
    attributes.fonts.xmobar = "xft:Iosevka:weight=Bold:size=10";
    attributes.fonts.dmenu = "xft:IosevkaCC:style=Bold:pixelsize=12";
    attributes.fonts.xmonadDefault = "xft:Iosevka:weight=Bold:size=16";
    home-manager.users."${config.attributes.mainUser.name}" = {
      gtk.font = lib.optionalAttrs (config.custom.appearance.gtk.enable) {
        package = pkgs.iosevka;
        name = "Iosevka Bold 10";
      };
      programs.alacritty.settings.font = {
        normal = {
          family = "Iosevka";
          style = "Bold";
        };
        bold = {
          family = "Iosevka";
          style = "Bold";
        };
        italic = {
          family = "Iosevka";
          style = "Italic";
        };
        size = 11.0;
      };
      programs.zathura.options.font = "Iosevka Bold 10";
      services.dunst.settings.global.font = "Iosevka Bold 10";
      xresources.properties = {
        "Emacs*XlwMenu.font" = "Iosevka:weight=Bold:size=14";
        "Emacs.Font" = "Iosevka:weight=Bold:size=14";
        "Emacs.dialog*.font" = "Iosevka:weight=Bold:size=14";

        "Xmessage*faceName" = "Iosevka";
        "Xmessage*faceSize" = "16";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "Iosevka:weight=Bold:size=16";
      };
    };
  };
}
