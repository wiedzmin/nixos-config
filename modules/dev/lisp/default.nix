{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.lisp;
in
{
  options = {
    dev.lisp = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs misc extensions.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.melpaStablePackages.slime
      ];
      ide.emacs.core.config = builtins.readFile ./elisp/lisp.el;
    })
  ];
}
