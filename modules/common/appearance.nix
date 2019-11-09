{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.appearance;
  rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
    ${pkgs.feh}/bin/feh --bg-fill ${cfg.wallpaper.root}/${cfg.wallpaper.current}
  '';
  emacsAppearanceSetup = ''
    (use-package avoid
      :custom
      (mouse-avoidance-mode 'jump))

    (use-package doom-modeline
      :ensure t
      :hook
      (after-init-hook . doom-modeline-init)
      :custom
      (doom-modeline-height 25)
      (doom-modeline-icon t)
      (doom-modeline-major-mode-icon nil)
      (doom-modeline-minor-modes nil))

    (use-package default-text-scale
      :ensure t
      :bind
      ("C-=" . default-text-scale-increase)
      ("C--" . default-text-scale-decrease)
      :custom
      (default-text-scale-amount 10)
      :config
      (default-text-scale-mode 1))

    (use-package hl-line
      :config
      (global-hl-line-mode 1))

    (use-package time
      :config
      (display-time)
      :custom
      (display-time-day-and-date t)
      (display-time-form-list (list 'time 'load))
      (display-time-world-list
       '(("${config.time.timeZone}" "${config.time.timeZone}")))
      (display-time-mail-file t)
      (display-time-default-load-average nil)
      (display-time-24hr-format t)
      (display-time-string-forms '( day " " monthname " (" dayname ") " 24-hours ":" minutes)))

    (use-package unicode-fonts
      :ensure t
      :after persistent-soft
      :hook
      (after-init-hook . unicode-fonts-setup))

    (use-package uniquify
      :custom
      (uniquify-buffer-name-style 'post-forward)
      (uniquify-separator ":")
      (uniquify-ignore-buffers-re "^\\*")
      (uniquify-strip-common-suffix nil))

    (use-package diredfl
      :ensure t
      :hook
      (dired-mode . diredfl-mode))

    (use-package rainbow-mode
      :ensure t
      :hook (css-mode-hook . rainbow-mode))
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
      fonts.default.monospace = mkOption {
        type = types.str;
        default = "Iosevka";
        description = "Default monospace font family.";
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
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = cfg.fonts.console != "";
        message = "appearance: must provide console font name.";
      }];

      environment.systemPackages = with pkgs; [
        screenfetch
      ];

      fonts = {
        fontconfig = {
          enable = true;
          antialias = cfg.fonts.antialias;
          dpi = cfg.fonts.dpi;
          defaultFonts = { monospace = [ cfg.fonts.default.monospace ]; };
        };
        enableFontDir = true;
        enableGhostscriptFonts = true;
        enableDefaultFonts = true;
      };
      i18n = {
        consoleFont = cfg.fonts.console;
        defaultLocale = cfg.fonts.locale;
        consoleUseXkbConfig = true;
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
    (mkIf (cfg.enable && cfg.wallpaper.enable) {
      assertions = [{
        assertion = cfg.wallpaper.root != "" && cfg.wallpaper.current != "";
        message = "appearance: must provide wallpapers path and image to use.";
      }];

      environment.systemPackages = with pkgs; [
        rescale-wallpaper
      ];

      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.autorandr.hooks = {
          postswitch = { "rescale-wallpaper" = "${rescale-wallpaper}/bin/rescale-wallpaper"; };
        };
        programs.feh.enable = true;
      };
    })
    (mkIf (cfg.enable && cfg.gtk.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        gtk.enable = true;
      };
    })
    (mkIf (cfg.enable && cfg.xresources.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        xresources.properties = {
          "Xmessage*Buttons" = "Quit";
          "Xmessage*defaultButton" = "Quit";
          "Xmessage*international" = true;

          "Emacs.FontBackend" = "xft,x";
          "Emacs.menuBar" = "0";
          "Emacs.toolBar" = "0";
          "Emacs.verticalScrollBars" = false;

          "urgentOnBell" = true;
          "visualBell" = true;

          "Xft.antialias" = true;
          "Xft.autohint" = false;
          "Xft.dpi" = "120.0";
          "Xft.hinting" = true;
          "Xft.hintstyle" = "hintslight";
          "Xft.lcdfilter" = "lcddefault";
          "Xft.rgba" = "none";
        };
        home.activation.xrdb = {
          after = ["linkGeneration"];
          before = [];
          data = "DISPLAY=:0 ${pkgs.xorg.xrdb}/bin/xrdb /home/${config.attributes.mainUser.name}/.Xresources";
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [
          epkgs.diredfl
          epkgs.doom-modeline
          epkgs.rainbow-mode
          epkgs.unicode-fonts
        ];
      };
      ide.emacs.config = ''${emacsAppearanceSetup}'';
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-M1-q" = ''spawn "${pkgs.xorg.xrdb}/bin/xrdb $HOME/.Xresources"'';
      };
    })
  ];
}
