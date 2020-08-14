{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.appearance;
  prefix = config.wmCommon.prefix;
  rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
    ${pkgs.feh}/bin/feh --bg-fill ${cfg.wallpaper.root}/${cfg.wallpaper.current}
  '';
in {
  options = {
    custom.appearance = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable appearance customizations.";
      };
      fonts.antialias = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable fonts antialiasing.";
      };
      fonts.dpi = mkOption {
        type = types.int;
        default = 115;
        description = "Font DPI.";
      };
      fonts.console = mkOption {
        type = types.str;
        default = "";
        description = "Font for console.";
      };
      fonts.locale = mkOption {
        type = types.str;
        default = "ru_RU.UTF-8";
        description = "Locale name.";
      };
      gtk.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable home-manager gtk module.";
      };
      xresources.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable home-manager xresources module.";
      };
      wallpaper.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable wallpapers functionality.";
      };
      wallpaper.root = mkOption {
        type = types.str;
        default = "";
        description = "Wallpapers root directory.";
      };
      wallpaper.current = mkOption {
        type = types.str;
        default = "";
        description = "Current wallpaper.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs appearance setup.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = cfg.fonts.console != "";
        message = "appearance: must provide console font name.";
      }];

      environment.systemPackages = with pkgs; [ screenfetch ];

      fonts = {
        fontconfig = {
          enable = true;
          antialias = cfg.fonts.antialias;
          dpi = cfg.fonts.dpi;
        };
        enableFontDir = true;
        enableGhostscriptFonts = true;
        enableDefaultFonts = true;
      };
      console = {
        font = cfg.fonts.console;
        useXkbConfig = true;
      };
      i18n = {
        defaultLocale = cfg.fonts.locale;
        inputMethod = {
          enabled = "ibus";
          ibus.engines = with pkgs.ibus-engines; [
            table
            table-others # for LaTeX input
            m17n
          ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.wallpaper.enable) { # TODO: review/try services.xserver.desktopManager.wallpaper.*
      assertions = [{
        assertion = cfg.wallpaper.root != "" && cfg.wallpaper.current != "";
        message = "appearance: must provide wallpapers path and image to use.";
      }];

      environment.systemPackages = with pkgs; [ rescale-wallpaper ];

      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.autorandr.hooks = {
          postswitch = { "rescale-wallpaper" = "${rescale-wallpaper}/bin/rescale-wallpaper"; };
        };
        programs.feh.enable = true;
      };
    })
    (mkIf (cfg.enable && cfg.gtk.enable) {
      programs.dconf.enable = true;
      services.dbus.packages = with pkgs; [ gnome3.dconf ];
      home-manager.users."${config.attributes.mainUser.name}" = { gtk.enable = true; };
    })
    (mkIf (cfg.enable && cfg.xresources.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        xresources.properties = {
          "Xmessage*Buttons" = "Quit";
          "Xmessage*defaultButton" = "Quit";
          "Xmessage*international" = true;

          "urgentOnBell" = true;
          "visualBell" = true;

          "Xft.antialias" = true;
          "Xft.autohint" = false;
          "Xft.dpi" = "120";
          "Xft.hinting" = true;
          "Xft.hintstyle" = "hintmedium";
        };
        home.activation.xrdb = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = "DISPLAY=:0 ${pkgs.xorg.xrdb}/bin/xrdb ${homePrefix ".Xresources"} || exit 0";
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.extraPackages = epkgs: [ epkgs.diredfl epkgs.doom-modeline epkgs.rainbow-mode epkgs.unicode-fonts ];
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./emacs/appearance.el; }));
    })
    (mkIf (cfg.enable && cfg.emacs.enable && cfg.xresources.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        xresources.properties = {
          "Emacs.menuBar" = "0";
          "Emacs.toolBar" = "0";
          "Emacs.verticalScrollBars" = false;
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ prefix "Alt" "q" ];
        cmd = "${pkgs.xorg.xrdb}/bin/xrdb $HOME/.Xresources";
        mode = "root";
      }];
    })
  ];
}
