{ config, lib, pkgs, ... }:

with lib;

# TODO: dunst
# TODO: gtk*

let
  cfg = config.themes.zenburn;
in {
  options.themes.zenburn = {
    enable = mkEnableOption "zenburn";
  };

  config = mkIf cfg.enable {
    home-manager.users."${config.attributes.mainUser.name}" = {
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
    };
  };
}
