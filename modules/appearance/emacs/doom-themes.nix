{ config, lib, ... }:
with lib;

let
  cfg = config.appearance.emacs.themes.doom;
  themesListMain = [
    "doom-one"
    "doom-one-light"
    "doom-vibrant"
    "doom-1337"
    "doom-acario-dark"
    "doom-acario-light"
    "doom-ayu-mirage"
    "doom-ayu-light"
    "doom-badger"
    "doom-challenger-deep"
    "doom-city-lights"
    "doom-dark+"
    "doom-dracula"
    "doom-ephemeral"
    "doom-fairy-floss"
    "doom-flatwhite"
    "doom-gruvbox-light"
    "doom-gruvbox"
    "doom-henna"
    "doom-homage-black"
    "doom-homage-white"
    "doom-horizon"
    "doom-Iosvkem"
    "doom-ir-black"
    "doom-laserwave"
    "doom-manegarm"
    "doom-material"
    "doom-material-dark"
    "doom-meltbus"
    "doom-miramare"
    "doom-molokai"
    "doom-monokai-classic"
    "doom-monokai-pro"
    "doom-monokai-machine"
    "doom-monokai-octagon"
    "doom-monokai-ristretto"
    "doom-monokai-spectrum"
    "doom-moonlight"
    "doom-nord-light"
    "doom-nord"
    "doom-nova"
    "doom-oceanic-next"
    "doom-old-hope"
    "doom-opera-light"
    "doom-opera"
    "doom-outrun-electric"
    "doom-palenight"
    "doom-peacock"
    "doom-plain-dark"
    "doom-plain"
    "doom-rouge"
    "doom-shades-of-purple"
    "doom-snazzy"
    "doom-solarized-dark"
    "doom-solarized-dark-high-contrast"
    "doom-solarized-light"
    "doom-sourcerer"
    "doom-spacegrey"
    "doom-tokyo-night"
    "doom-tomorrow-day"
    "doom-tomorrow-night"
    "doom-wilmersdorf"
    "doom-xcode"
    "doom-zenburn"
  ];
  themesListTreemacs = [
    "doom-atom"
    "doom-colors" # for less minimal icon theme
  ];
in {
  options = {
    appearance.emacs.themes.doom = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Doom themes.";
      };
      themes.main = mkOption {
        type = types.enum themesListMain;
        default = "doom-one";
        description = "Doom theme to use";
      };
      themes.treemacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable `treemacs` theming";
      };
      themes.treemacs.name = mkOption {
        type = types.enum themesListTreemacs;
        default = "doom-atom";
        description = "Doom theme to use for `treemacs`";
      };
      themes.neotree.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Doom theme to use for `neotree`";
      };
      faces.bold.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable bold faces";
      };
      faces.italic.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable italic faces";
      };
      visualBell.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable flashing mode-line on errors";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && config.wm.i3.enable) {
      ide.emacs.core.extraPackages = epkgs:
        [ epkgs.doom-themes ] ++ lib.optionals (cfg.themes.neotree.enable) [ epkgs.all-the-icons ];
      ide.emacs.core.config = lib.optionalString (cfg.themes.neotree.enable) ''
        (use-package all-the-icons)
      '' + ''
        (use-package doom-themes
          :custom
          (doom-themes-enable-bold ${if cfg.faces.bold.enable then "t" else "nil"})
          (doom-themes-enable-italic ${if cfg.faces.italic.enable then "t" else "nil"})
          :config
          (load-theme '${cfg.themes.main} t)
          ${lib.optionalString (cfg.visualBell.enable) "(doom-themes-visual-bell-config)"}
          ${lib.optionalString (cfg.themes.neotree.enable) "(doom-themes-neotree-config)"}
          ${lib.optionalString (cfg.themes.treemacs.enable) ''
            (setq doom-themes-treemacs-theme "${cfg.themes.treemacs}")
            (doom-themes-treemacs-config)
          ''}
          (doom-themes-org-config))
      '';
    })
  ];
}
