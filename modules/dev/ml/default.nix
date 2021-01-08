{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.ml;
  user = config.attributes.mainUser.name;
in {
  options = {
    dev.ml = {
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.emacs.enable {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.tuareg ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/ml.el;
    })
  ];
}
