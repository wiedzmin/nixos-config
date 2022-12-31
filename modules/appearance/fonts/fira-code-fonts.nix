{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.appearance.fonts.fira-code;
  user = config.attributes.mainUser.name;
  inherit (config.appearance.fonts) beautify;
in
{
  options.appearance.fonts.fira-code = { enable = mkEnableOption "fira-code"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [ fira-code ];
      fontconfig = { defaultFonts = { monospace = [ "Fira Code" ]; }; };
    };
    wmCommon.fonts.default = "pango:${if beautify then "FiraCode Nerd Font " else "Fira Code "}Bold 8";
    wmCommon.fonts.simple = "${if beautify then "FiraCode Nerd Font " else "Fira Code "}Bold 8";
    wmCommon.fonts.dmenu = "Fira Code:bold:pixelsize=12";
    wmCommon.fonts.statusbar = "pango:${if beautify then "FiraCode Nerd Font " else "Fira Code "}Bold 8";
    shell.core.variables = [{ TB_SELECTOR_FONT = "Fira Code:bold:pixelsize=12"; global = true; }];
    home-manager.users."${user}" = {
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
      programs.kitty.settings = {
        # If Regular is not distinguishable, use Medium
        font_family = "Fira Code Light";
        bold_font = "Fira Code Regular";
        italic_font = "Fira Code Light Italic";
        bold_italic_font = "Fira Code Regular Italic";
        font_size = "9.0";
      };
      programs.rofi.font = "Fira Code Bold 11";
      programs.zathura.options.font = "Fira Code Bold 8";
      services.dunst.settings.global.font = "Fira Code Bold 8";
      xresources.properties = {
        "Xmessage*faceName" = "Fira Code";
        "Xmessage*faceSize" = "12";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "Fira Code:weight=Bold:size=12";
      };
    };
    ide.emacs.core.config = ''
      (defun custom/set-font (frame)
        "Configure faces on frame creation"
        (select-frame frame)
        (if (display-graphic-p)
            (set-frame-font "Fira Code ExtraBold 8" nil t)))
      (add-hook 'after-make-frame-functions #'custom/set-font)
    '';
  };
}
