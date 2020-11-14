{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.themes.fonts.iosevka;
  user = config.attributes.mainUser.name;
  beautify = config.custom.appearance.fonts.beautify;
in {
  options.themes.fonts.iosevka = { enable = mkEnableOption "iosevka"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [ iosevka ];
      fontconfig = { defaultFonts = { monospace = [ "Iosevka" ]; }; };
    };
    wmCommon.fonts.default = "pango:Iosevka ${if beautify then "Nerd Font " else ""}Bold 9";
    wmCommon.fonts.dmenu = "Iosevka:bold:size=9";
    wmCommon.fonts.statusbar = "pango:Iosevka ${if beautify then "Nerd Font " else ""}Bold 9";
    home-manager.users.${user} = {
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
        "Emacs.Font" = "Iosevka:weight=Bold:size=14";

        "Xmessage*faceName" = "Iosevka";
        "Xmessage*faceSize" = "16";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "Iosevka:weight=Bold:size=16";
      };
    };
  };
}
