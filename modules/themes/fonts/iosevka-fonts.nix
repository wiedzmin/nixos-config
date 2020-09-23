{ config, lib, pkgs, ... }:

with lib;
let cfg = config.themes.fonts.iosevka;
in {
  options.themes.fonts.iosevka = { enable = mkEnableOption "iosevka"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [ iosevka ];
      fontconfig = { defaultFonts = { monospace = [ "Iosevka" ]; }; };
    };
    wmCommon.fonts.default = "pango:Iosevka Bold 9";
    wmCommon.fonts.dmenu = "Iosevka:bold:size=9";
    wmCommon.fonts.statusbar = "pango:Iosevka Bold 9";
    ide.emacs.fontSpec = "Iosevka:size=14";
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
        "Xmessage*faceName" = "Iosevka";
        "Xmessage*faceSize" = "16";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "Iosevka:weight=Bold:size=16";
      };
    };
  };
}
