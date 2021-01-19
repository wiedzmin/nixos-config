{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ide.emacs.history;
  user = config.attributes.mainUser.name;
in {
  options = {
    ide.emacs.history = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable history maintaining extensions.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "emacs/history: core configuration must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.backup-each-save
        epkgs.recentf-ext
        epkgs.savekill
        epkgs.super-save
      ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./history.el;
    })
  ];
}
