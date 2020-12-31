{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ide.emacs.navigation;
  user = config.attributes.mainUser.name;
in {
  options = {
    ide.emacs.navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs navigation extensions.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "emacs: core configuration must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.amx
        epkgs.beginend
        epkgs.flycheck-projectile
        epkgs.mwim
        epkgs.rainbow-delimiters
        epkgs.recursive-narrow
      ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./navigation.el;
    })
  ];
}
