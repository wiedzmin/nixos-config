{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.appearance.fonts.jetbrains-mono;
  user = config.attributes.mainUser.name;
  inherit (config.appearance.fonts) beautify;
in
{
  options.appearance.fonts.jetbrains-mono = { enable = mkEnableOption "jetbrains-mono"; };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [ jetbrains-mono ];
      fontconfig = { defaultFonts = { monospace = [ "JetBrains Mono" ]; }; };
    };
    wmCommon.fonts.default = "pango:${if beautify then "JetBrainsMono Nerd Font " else "JetBrains Mono "}Bold 8";
    wmCommon.fonts.simple = "${if beautify then "JetBrainsMono Nerd Font " else "JetBrains Mono "}Bold 8";
    wmCommon.fonts.dmenu = "JetBrains Mono:bold:size=8";
    wmCommon.fonts.statusbar = "pango:${if beautify then "JetBrainsMono Nerd Font " else "JetBrains Mono "}Bold 8";
    shell.core.variables = [{ TB_SELECTOR_FONT = "JetBrains Mono:bold:size=8"; global = true; }];
    home-manager.users."${user}" = {
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
      programs.kitty.settings = {
        # If Regular is not distinguishable, use Medium
        font_family = "JetBrains Mono Light";
        bold_font = "JetBrains Mono Regular";
        italic_font = "JetBrains Mono Light Italic";
        bold_italic_font = "JetBrains Mono Regular Italic";
        font_size = "9.0";
      };
      programs.rofi.font = "JetBrains Mono Bold 11";
      programs.zathura.options.font = "JetBrains Mono Bold 9";
      services.dunst.settings.global.font = "JetBrains Mono Bold 8";
      xresources.properties = {
        "Xmessage*faceName" = "JetBrains Mono";
        "Xmessage*faceSize" = "12";
        "Xmessage*faceWeight" = "Bold";

        "dzen2.font" = "JetBrains Mono:weight=Bold:size=12";
      } // lib.optionalAttrs (!config.ide.emacs.core.useModernDrawingLibs) {
        "Emacs.Font" = "JetBrains Mono:weight=Bold:size=12";
      };
    };
    ide.emacs.core.config = lib.optionalString config.ide.emacs.core.useModernDrawingLibs ''
      (defun custom/set-font (frame)
        "Configure faces on frame creation"
        (select-frame frame)
        (if (display-graphic-p)
            (set-frame-font "JetBrains Mono ExtraBold 8" nil t)))
      (add-hook 'after-make-frame-functions #'custom/set-font)
    '';
  };
}
