{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.git.navigation;
  user = config.attributes.mainUser.name;
in
{
  options = {
    dev.git.navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git navigation setup.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs navigation-related git setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.emacs.enable) {
      assertions = [
        {
          assertion = config.dev.git.core.enable;
          message = "dev/git/navigation: core configuration must be enabled.";
        }
        {
          assertion = config.ide.emacs.navigation.enable;
          message = "dev/git/navigation/emacs: ide/emacs/navigation must be enabled.";
        }
      ];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.dired-git-info
        epkgs.git-timemachine
        epkgs.git-walktree
        epkgs.magit-todos
      ];
      ide.emacs.core.config = builtins.readFile ./elisp/navigation.el;
    })
  ];
}
