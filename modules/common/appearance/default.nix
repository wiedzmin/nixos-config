{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.appearance;
  user = config.attributes.mainUser.name;
  prefix = config.wmCommon.prefix;
  rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
    ${pkgs.feh}/bin/feh --bg-${cfg.wallpaper.transform} ${cfg.wallpaper.root}/${cfg.wallpaper.current}
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
      wallpaper.transform = mkOption {
        default = "fill";
        type = types.enum [ "fill" "max" "scale" "tile" ];
        description = ''
          `fill`: Like `scale`, but preserves aspect ratio by zooming the image until it fits.
                  Either a horizontal or a vertical part of the image will be cut off.
          `max`: Like `fill`, but scale the image to the maximum size that fits the screen with borders on one side.
                 The border color can be set using --image-bg.
          `scale`: Fit the file into the background without repeating it, cutting off stuff or using borders.
                   But the aspect ratio is not preserved either.
          `tile`: Tile (repeat) the image in case it is too small for the screen
        '';
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
        fontDir.enable = true;
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

      home-manager.users.${user} = {
        programs.autorandr.hooks = {
          postswitch = { "rescale-wallpaper" = "${rescale-wallpaper}/bin/rescale-wallpaper"; };
        };
        programs.feh.enable = true;
      };
    })
    (mkIf (cfg.enable && cfg.gtk.enable) {
      programs.dconf.enable = true;
      services.dbus.packages = with pkgs; [ gnome3.dconf ];
      home-manager.users.${user} = { gtk.enable = true; };
    })
    (mkIf (cfg.enable && cfg.xresources.enable) {
      home-manager.users.${user} = {
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
      ide.emacs.extraPackages = epkgs: [ epkgs.diredfl epkgs.rainbow-mode epkgs.unicode-fonts ];
      ide.emacs.config = readSubstituted ../subst.nix ./emacs/appearance.el;
    })
    (mkIf (cfg.enable && cfg.emacs.enable && cfg.xresources.enable) {
      home-manager.users.${user} = {
        xresources.properties = {
          "Emacs.fontBackend" = "xft,x";
          "Emacs.menuBar" = "0";
          "Emacs.toolBar" = "0";
          "Emacs.verticalScrollBars" = false;
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ prefix "Alt" "q" ];
          cmd = "${pkgs.xorg.xrdb}/bin/xrdb $HOME/.Xresources";
          mode = "root";
        }
        {
          key = [ "s" ];
          cmd = "${rescale-wallpaper}/bin/rescale-wallpaper";
          mode = "xserver";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ rescale-wallpaper ]; };
    })
  ];
}
