{ config, lib, pkgs, ... }:
with lib;

let cfg = config.appearance.emacs.themes.dracula;
in
{
  options = {
    appearance.emacs.themes.dracula = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Dracula theme.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.dracula-theme ];
      ide.emacs.core.config = ''
        (use-package dracula-theme
          :hook
          (after-init-hook . (lambda () (load-theme 'dracula t))))
      '';
    })
  ];
}
