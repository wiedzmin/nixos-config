{ config, lib, ... }:

with lib;

let
  cfg = config.appearance.colors.zenburn;
  user = config.attributes.mainUser.name;
in
{
  options.appearance.colors.zenburn = { enable = mkEnableOption "zenburn"; };

  config = mkIf cfg.enable {
    home-manager.users."${user}" = {
      xdg.configFile."quassel-irc.org/settings.qss".source = ./assets/zenburn.qss;
      programs.zathura.options = {
        completion-bg = "#404040";
        completion-fg = "#7cb8bb";
        completion-highlight-bg = "#7cb8bb";
        completion-highlight-fg = "#ffffff";
        default-bg = "#383838";
        default-fg = "#404040";
        highlight-active-color = "#7cb8bb";
        highlight-color = "#e0cf9f";
        inputbar-bg = "#383838";
        inputbar-fg = "#ffffff";
        notification-bg = "#383838";
        notification-error-bg = "#383838";
        notification-error-fg = "#dca3a3";
        notification-fg = "#ffffff";
        notification-warning-bg = "#383838";
        notification-warning-fg = "#dca3a3";
        recolor = false;
        recolor-darkcolor = "#c0c0c0";
        recolor-keephue = false;
        recolor-lightcolor = "#383838";
        statusbar-bg = "#606060";
        statusbar-fg = "#808080";
      };
      programs.rofi.theme = ./assets/base16-zenburn.rasi;
      services.dunst.settings = {
        urgency_low = {
          background = "#333333";
          foreground = "#EFEFEF";
        };
        urgency_normal = {
          background = "#333333";
          foreground = "#58afb3";
        };
        urgency_critical = {
          background = "#333333";
          foreground = "#ff7b00";
          frame_color = "#D64E4E";
        };
      };
      programs.kitty = {
        themeFile = "zenburned";
      };
    };
    controlcenter.lnc.theme = {
      foregroundNormal = "#B9967F";
      foregroundCritical = "#B9967F";
      backgroundNormal = "#404040";
      foregroundNormalNC = "#B9967F";
      foregroundCriticalNC = "#B9967F";
      backgroundNormalNC = "#404040";
    };
    wm.i3.theme = {
      client = ''
        #                       border  bground text
        client.focused          #dbdbdb #F5DEB3 #1E1E1E
        client.focused_inactive #333333 #0F0F0F #E3CCA1
        client.unfocused        #333333 #010101 #85753A
        client.urgent           #856E23 #900000 #F5DEB3
      '';
      bar = ''
        background #0F0F0F
        statusline #F5DEB3

        focused_workspace  #F5DEB3 #666666
        active_workspace   #F5DEB3 #0F0F0F
        inactive_workspace #F5DEB3 #1E1E1E
        urgent_workspace   #ffffff #900000
      '';
      i3status-rs = {
        theme = "gruvbox-dark";
        overrides = {
          alternating_tint_bg = "#0f0f0f";
          alternating_tint_fg = "#282828";
          critical_fg = "#282828";
          good_fg = "#282828";
          warning_fg = "#282828";
        };
      };
    };
  };
}
