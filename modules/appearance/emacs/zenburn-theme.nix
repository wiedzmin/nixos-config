{ config, lib, pkgs, ... }:
with lib;

let cfg = config.appearance.emacs.themes.zenburn;
in
{
  options = {
    appearance.emacs.themes.zenburn = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable zenburn theme.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.zenburn-theme ];
      ide.emacs.core.config = ''
        (use-package zenburn-theme
          :hook
          (after-init-hook . (lambda () (load-theme 'zenburn t))))
      '';
    })
  ];
}
