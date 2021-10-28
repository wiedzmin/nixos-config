{ config, lib, pkgs, ... }:
with lib;

let cfg = config.appearance.emacs.themes.modus;
in {
  options = {
    appearance.emacs.themes.modus = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Modus themes.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && config.wm.i3.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.modus-themes ];
      ide.emacs.core.config = ''
        (use-package modus-themes
          :init
          (setq modus-themes-bold-constructs t)
          (setq modus-themes-mode-line '3d)
          (modus-themes-load-themes)
          (modus-themes-load-vivendi)
          :bind
          (:map dbFile
                ("t" . modus-themes-toggle)))
      '';
    })
  ];
}
