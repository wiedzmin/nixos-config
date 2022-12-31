{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.appearance.fonts.hack;
  user = config.attributes.mainUser.name;
  inherit (config.appearance.fonts) beautify;
in
{
  options.appearance.fonts.hack = { enable = mkEnableOption "hack"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [ hack-font ];
      fontconfig = { defaultFonts = { monospace = [ "Hack" ]; }; };
    };
    wmCommon.fonts.default = "pango:Hack ${if beautify then "Nerd Font " else ""}Bold 8";
    wmCommon.fonts.simple = "Hack ${if beautify then "Nerd Font " else ""}Bold 8";
    wmCommon.fonts.dmenu = "Hack:bold:size=8";
    wmCommon.fonts.statusbar = "pango:Hack ${if beautify then "Nerd Font " else ""}Bold 8";
    shell.core.variables = [{ TB_SELECTOR_FONT = "Hack:bold:size=8"; global = true; }];
    home-manager.users."${user}" = {
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
      programs.kitty.settings = {
        # If Regular is not distinguishable, use Medium
        font_family = "Hack Light";
        bold_font = "Hack Regular";
        italic_font = "Hack Light Italic";
        bold_italic_font = "Hack Regular Italic";
        font_size = "9.0";
      };
      programs.rofi.font = "Hack Bold 11";
      programs.zathura.options.font = "Hack Bold 10";
      services.dunst.settings.global.font = "Hack Bold 10";
      xresources.properties = {
        "Xmessage*faceName" = "Hack";
        "Xmessage*faceSize" = "16";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "Hack:weight=Bold:size=16";
      };
    };
    ide.emacs.core.config = ''
      (defun custom/set-font (frame)
        "Configure faces on frame creation"
        (select-frame frame)
        (if (display-graphic-p)
            (set-frame-font "Hack ExtraBold 10" nil t)))
      (add-hook 'after-make-frame-functions #'custom/set-font)
    '';
  };
}
