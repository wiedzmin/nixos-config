{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ide.emacs.history;
in
{
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
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "emacs/history: core configuration must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.backup-each-save
        epkgs.recentf-ext
        epkgs.savekill
      ];
      ide.emacs.core.config = builtins.readFile ./elisp/history.el;
    })
  ];
}
