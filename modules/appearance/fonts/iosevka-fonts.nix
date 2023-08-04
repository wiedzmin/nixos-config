{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# TODO: add `magnified` bool option, that will result in slightly incremented font sizes (for all fonts)

let
  cfg = config.appearance.fonts.iosevka;
  user = config.attributes.mainUser.name;
  inherit (config.appearance.fonts) beautify;
  baseFont = {
    family = "Iosevka";
    style = "Bold";
    size = 8;
  };
  fontBeautified = baseFont // { family = "Iosevka ${if beautify then "Nerd Font" else ""}"; };
  fontRofi = baseFont // { size = 10; };
  fontDunst = baseFont // { size = 9; };
  fontXresources = baseFont // { size = 12; };
  sizeAlacritty = 9.0;
  sizeKitty = 9.0;
in
{
  options = {
    appearance.fonts.iosevka = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Iosevka font";
      };
      comfy.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use Comfy version from Prot";
      };
    };
  };

  config = mkIf cfg.enable {
    fonts = {
      packages = with pkgs; lib.optionals (!cfg.comfy.enable) [ iosevka ] ++
        lib.optionals (cfg.comfy.enable) [ iosevka-comfy.comfy ];
      fontconfig = { defaultFonts = { monospace = [ baseFont.family ]; }; };
    };
    wmCommon.fonts.default = makeFontStrPango fontBeautified;
    wmCommon.fonts.simple = makeFontStrSimple fontBeautified;
    wmCommon.fonts.dmenu = makeFontStrColons baseFont;
    wmCommon.fonts.statusbar = makeFontStrPango fontBeautified;
    shell.core.variables = [{ TB_SELECTOR_FONT = makeFontStrColons baseFont; global = true; }];
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
      services.dunst.settings.global.font = makeFontStrSimple fontDunst;
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
            (set-frame-font "${makeFontStrSimple baseFont}" nil t)))
      (add-hook 'after-make-frame-functions #'custom/set-font)
    '';
  };
}
