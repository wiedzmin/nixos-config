{ config, lib, pkgs, ... }:
with lib;

let cfg = config.appearance.emacs.modeline.telephone;
in {
  options = {
    appearance.emacs.modeline.telephone = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable telephone-line.";
      };
      height = mkOption {
        type = types.int;
        default = 24;
        description = "Modeline height in pixels.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && config.wm.i3.enable) {
      ide.emacs.extraPackages = epkgs: [ epkgs.telephone-line ];
      ide.emacs.config = ''
        (use-package telephone-line
          :hook
          (after-init-hook . (lambda () (telephone-line-mode 1)))
          :custom
          (telephone-line-primary-left-separator 'telephone-line-cubed-left)
          (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
          (telephone-line-primary-right-separator 'telephone-line-cubed-right)
          (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
          (telephone-line-height ${builtins.toString cfg.height})
          (telephone-line-lhs
           '((evil   . (telephone-line-buffer-modified-segment))
             (accent . (telephone-line-narrow-segment
                        telephone-line-position-segment
                        telephone-line-vc-segment))
             (nil    . (telephone-line-projectile-buffer-segment))))
          (telephone-line-rhs
           '((nil    . (telephone-line-minor-mode-segment))
             (accent . (telephone-line-major-mode-segment
                        telephone-line-flycheck-segment))
             (evil   . (telephone-line-misc-info-segment)))))
      '';
    })
  ];
}
