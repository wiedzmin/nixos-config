{ config, lib, ... }:

with lib;

let
  cfg = config.appearance.colors.zenburn;
  user = config.attributes.mainUser.name;
in {
  options.appearance.colors.zenburn = { enable = mkEnableOption "zenburn"; };

  config = mkIf cfg.enable {
    home-manager.users."${user}" = {
      xdg.configFile."quassel-irc.org/settings.qss".source = ./zenburn.qss;
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
    };
  };
}
