{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.lisp;
in {
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
    (mkIf (cfg.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.melpaStablePackages.slime
      ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/lisp.el;
    })
  ];
}

