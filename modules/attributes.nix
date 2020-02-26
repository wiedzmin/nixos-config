{ config, lib, pkgs, ... }:

with lib;

{
  options.attributes = {
    mainUser.name = mkOption {
      description = "Main user to be granted various service-related rights to";
      type = types.str;
    };
    mainUser.fullName = mkOption {
      description = "Main user's full name";
      type = types.str;
    };
    mainUser.email = mkOption {
      description = "Main user's email";
      type = types.str;
    };
    mainUser.gpgKeyID = mkOption {
      description = "Main user's GPG key ID";
      type = types.str;
    };
    localGroup = mkOption {
      description = "Default local group name";
      type = types.str;
      default = "users";
    };
    fonts.basic.package = mkOption {
      type = types.package;
      example = literalExample "pkgs.iosevka";
      description = "The package for font to be used as basic/default in certain cases.";
    };
    fonts.basic.xft = mkOption {
      type = types.str;
      description = "The XFT font definition to be used as basic/default in certain cases.";
    };
    fonts.basic.raw = mkOption {
      type = types.str;
      description = "The 'raw' font definition to be used as basic/default in certain cases.";
    };
    # TODO: think if this is the proper location
    fonts.xmobar = mkOption {
      description = "Font definition for XMobar";
      type = types.str;
    };
    # TODO: think if this is the proper location
    fonts.xmonadDefault = mkOption {
      description = "Default Font for XMonad";
      type = types.str;
    };
    # TODO: think if this is the proper location
    fonts.dmenu = mkOption {
      description = "Default Font for Dmenu";
      type = types.str;
    };
    defaultCommands.terminal = mkOption {
      description = "Default terminal";
      type = types.str;
      default = "${pkgs.alacritty}/bin/alacritty";
    };
    defaultCommands.browser = mkOption {
      description = "Default browser";
      type = types.str;
      default = "${pkgs.firefox-unwrapped}/bin/firefox --new-window";
    };
    defaultCommands.fallbackBrowser = mkOption {
      description = "Default browser to fallback to in some cases";
      type = types.str;
      default = "${pkgs.chromium}/bin/chromium";
    };
    defaultCommands.ebookReader = mkOption {
      description = "Default ebooks reader";
      type = types.str;
      default = "${pkgs.zathura}/bin/zathura";
    };
    defaultCommands.textProcessor = mkOption {
      description = "Default text processor app";
      type = types.str;
      default = "libreoffice";
    };
    defaultCommands.spreadsheetEditor = mkOption {
      description = "Default spreadheets editor";
      type = types.str;
      default = "libreoffice";
    };
    defaultCommands.pager = mkOption {
      description = "Default pager";
      type = types.str;
      default = "${pkgs.less}/bin/less";
    };
    defaultCommands.videoPlayer = mkOption {
      description = "Default video player";
      type = types.str;
      default = "mpv";
    };
    defaultCommands.imageViewer = mkOption {
      description = "Default image viewer";
      type = types.str;
      default = "";
    };
    tmux.defaultSession = mkOption {
      description = "Default tmux predefined session name to be used in automation scripts";
      type = types.str;
      default = "main";
    };
    tmux.paneHistoryDepthLines = mkOption {
      description = "Tmux pane history depth in lines";
      type = types.int;
      default = 10000;
    };
    dev.remoteCommands = mkOption {
      description = "Predefined commands list to execute remotely. Note that those must be present on ssh target.";
      type = types.listOf types.str;
      default = [ "ctop" "jnettop" ];
    };
    security.passwordStorePath = mkOption {
      description = "Default path to Pass password store";
      type = types.str;
      default = "/home/${config.attributes.mainUser.name}/.password-store";
    };
    debug.enable = mkOption {
      description = "Whether to enable unfinished/in-development/broken custom scripts/packages";
      type = types.bool;
      default = false;
    };
    hardware.cores = mkOption {
      description = "CPU cores count";
      type = types.int;
      default = 4;
    };
    hardware.monitors = {
      internalHead = {
        name = mkOption {
          type = types.str;
          default = "";
          description = "Internal laptop head name";
        };
        edid = mkOption {
          type = types.str;
          default = "";
          description = "Internal laptop head EDID value";
        };
        resolution = mkOption {
          type = types.str;
          default = "";
          description = "Internal laptop head resolution";
        };
      };
    };
  };
}
