{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.appearance.fonts.source-code-pro;
  user = config.attributes.mainUser.name;
  inherit (config.appearance.fonts) beautify;
  baseFont = {
    family = "Source Code Pro";
    style = "Bold";
    size = 8;
  };
  fontBeautified = baseFont // { family = if beautify then "SauceCodePro Nerd Font" else "Source Code Pro"; };
  fontDec1 = baseFont // { size = 7; };
  fontRofi = baseFont // { size = 9; };
  fontXresources = baseFont // { size = 12; };
  sizeAlacritty = 9.0;
  sizeKitty = 8.0;
in
{
  options.appearance.fonts.source-code-pro = { enable = mkEnableOption "source-code-pro"; };

  config = mkIf cfg.enable {
    fonts = {
      packages = with pkgs; [ source-code-pro ];
      fontconfig = { defaultFonts = { monospace = [ baseFont.family ]; }; };
    };
    wmCommon.fonts.default = makeFontStrPango fontBeautified;
    wmCommon.fonts.simple = makeFontStrSimple fontBeautified;
    wmCommon.fonts.dmenu = makeFontStrColons fontDec1;
    wmCommon.fonts.statusbar = makeFontStrPango fontBeautified;
    shell.core.variables = [{ TB_SELECTOR_FONT = makeFontStrColons fontDec1; global = true; }];
    home-manager.users."${user}" = {
      programs.alacritty.settings.font = {
        normal = {
          family = baseFont.family;
          style = baseFont.style;
        };
        bold = {
          family = baseFont.family;
          style = baseFont.style;
        };
        italic = {
          family = baseFont.family;
          style = "Italic";
        };
        size = sizeAlacritty;
      };
      programs.kitty.settings = {
        font_family = baseFont.family;
        bold_font = "auto";
        italic_font = "auto";
        bold_italic_font = "auto";
        font_size = builtins.toString sizeKitty;
      };
      programs.qutebrowser.extraConfig = ''
        c.fonts.completion.category = '${makeFontStrQB baseFont}'
        c.fonts.completion.entry = '${makeFontStrQB baseFont}'
        c.fonts.debug_console = '${makeFontStrQB baseFont}'
        c.fonts.downloads = '${makeFontStrQB baseFont}'
        c.fonts.hints = '${makeFontStrQB (baseFont // { size = 9; })}'
        c.fonts.keyhint = '${makeFontStrQB baseFont}'
        c.fonts.messages.error = '${makeFontStrQB baseFont}'
        c.fonts.messages.info = '${makeFontStrQB baseFont}'
        c.fonts.messages.warning = '${makeFontStrQB baseFont}'
        c.fonts.prompts = '${makeFontStrQB baseFont}'
        c.fonts.statusbar = '${makeFontStrQB baseFont}'
        c.fonts.tabs.selected = '${makeFontStrQB baseFont}'
        c.fonts.tabs.unselected = '${makeFontStrQB baseFont}'
      '';
      programs.rofi.font = makeFontStrSimple fontRofi;
      programs.zathura.options.font = makeFontStrSimple baseFont;
      services.dunst.settings.global.font = makeFontStrSimple baseFont;
      xresources.properties = {
        "Xmessage*faceName" = fontXresources.family;
        "Xmessage*faceSize" = builtins.toString fontXresources.size;
        "Xmessage*faceWeight" = fontXresources.style;

        "dzen2.font" = makeFontStrColons2 fontXresources;
      };
    };
    ide.emacs.core.config = ''
      (defun custom/set-font (frame)
        "Configure faces on frame creation"
        (select-frame frame)
        (if (display-graphic-p)
            (set-frame-font "${makeFontStrSimple fontDec1}" nil t)))
      (add-hook 'after-make-frame-functions #'custom/set-font)
    '';
  };
}
