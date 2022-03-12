{ config, lib, ... }:
with lib;

let cfg = config.appearance.emacs.themes.zenburn;
in
{
  options = {
    appearance.emacs.themes.zenburn = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Zenburn theme.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.hc-zenburn-theme ];
      ide.emacs.core.config = ''
        (use-package hc-zenburn-theme
          :hook
          (after-init-hook . (lambda () (load-theme 'hc-zenburn t))))
      '';
    })
  ];
}
