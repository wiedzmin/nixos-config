{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.git.navigation;
  user = config.attributes.mainUser.name;
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    dev.git.navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git navigation setup.";
      };
      ghq.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable ghq tooling.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs navigation-related git setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.ghq.enable) {
      environment.systemPackages = with pkgs; [ gitAndTools.ghq ];

      home-manager.users."${user}" = {
        programs.git.extraConfig = optionalAttrs config.navigation.bookmarks.enable {
          "ghq" = { root = config.navigation.bookmarks.workspaces.globalRoot; };
        };
        programs.zsh.shellAliases = { gg = "${pkgs.gitAndTools.ghq}/bin/ghq get"; };
        programs.fish.shellAliases = { gg = "${pkgs.gitAndTools.ghq}/bin/ghq get"; };

        xdg.configFile = optionalAttrs (config.shell.core.queueing.enable && config.completion.expansions.enable) {
          "espanso/match/git_navigation.yml".source = yaml.generate "espanso-git_navigation.yml" {
            matches = [
              {
                trigger = ":pgg";
                replace = "pueue add 'ghq get $|$'";
              }
            ];
          };
        };
      };
    })
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
    (mkIf (cfg.enable && cfg.ghq.enable && cfg.emacs.enable) {
      assertions = [
        {
          assertion = config.ide.emacs.navigation.enable;
          message = "dev/git/navigation/emacs: ide/emacs/navigation must be enabled.";
        }
      ];

      ide.emacs.core.extraPackages = epkgs: [ epkgs.consult-ghq ];
      ide.emacs.core.customPackages = {
        "ghq-tap" = { text = builtins.readFile ./elisp/custom/ghq-tap.el; };
      };
      ide.emacs.core.config = ''
        (use-package ghq-tap)
      '' + optionalString (config.ide.emacs.navigation.collections.backend == "consult") (builtins.readFile ./elisp/consult.el);
    })
  ];
}
