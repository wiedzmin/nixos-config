{ config, lib, pkgs, ... }:
with lib;

let cfg = config.appearance.emacs.modeline.doom;
in {
  options = {
    appearance.emacs.modeline.doom = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable doom-modeline.";
      };
      height = mkOption {
        type = types.int;
        default = 25;
        description = "Modeline height in pixels.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && config.wm.i3.enable) {
      ide.emacs.extraPackages = epkgs: [ epkgs.doom-modeline ];
      ide.emacs.config = ''
        (use-package doom-modeline
          :hook
          (after-init-hook . doom-modeline-init)
          :custom
          (doom-modeline-height ${builtins.toString cfg.height})
          (doom-modeline-icon t)
          (doom-modeline-major-mode-icon nil)
          (doom-modeline-minor-modes nil))
      '';
    })
  ];
}
