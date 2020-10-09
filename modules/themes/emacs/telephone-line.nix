{ config, lib, pkgs, ... }:
with lib;

let cfg = config.themes.emacs.modeline.telephone;
in {
  options = {
    themes.emacs.modeline.telephone = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable telephone-line.";
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
      ide.emacs.extraPackages = epkgs: [ epkgs.telephone-line ];
      ide.emacs.config = ''
        (use-package telephone-line
          :hook
          (after-init-hook . (lambda () (telephone-line-mode 1)))
          )
      '';
    })
  ];
}
