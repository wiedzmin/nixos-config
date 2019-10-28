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
    paths.nixpkgs = mkOption {
      description = "Full path to `Nixpkgs`";
      type = types.str;
      default = "/etc/nixos/pkgs/forges/github.com/NixOS/nixpkgs-channels";
    };
    paths.home-manager = mkOption {
      description = "Full path to `home-manager`";
      type = types.str;
      default = "/etc/nixos/pkgs/forges/github.com/rycee/home-manager";
    };
    defaultCommands.terminal = mkOption {
      description = "Default terminal";
      type = types.str;
      default = "${pkgs.alacritty}/bin/alacritty -e";
    };
    defaultCommands.remoteTerminal = mkOption {
      description = "Default remote terminal";
      type = types.str;
      default = "${pkgs.eternal-terminal}/bin/et";
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
      default = "${pkgs.most}/bin/most";
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
    staging.enable = mkOption {
      description = "Whether to enable staging packages/configuration";
      type = types.bool;
      default = false;
    };
    hardware.monitors = {
      internalHead = {
        name = mkOption {
          type = types.str ;
          default = "";
          description = "Internal laptop head name";
        };
        edid = mkOption {
          type = types.str ;
          default = "";
          description = "Internal laptop head EDID value";
        };
        resolution = mkOption {
          type = types.str ;
          default = "";
          description = "Internal laptop head resolution";
        };
      };
    };
  };
}
