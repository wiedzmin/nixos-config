{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.git.misc;
  user = config.attributes.mainUser.name;
  unstable-future = import inputs.unstable-future {
    config = config.nixpkgs.config // {
      allowUnfree = true;
      permittedInsecurePackages = config.ext.nix.core.permittedInsecurePackages;
    };
    localSystem = { system = "x86_64-linux"; };
  };
in
{
  options = {
    dev.git.misc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git miscellaneous setup.";
      };
      defaultUpstreamRemote = mkOption {
        type = types.str;
        default = "upstream";
        description = "Name of upstream repo remote.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ gitleaks gitnuro mgitstatus difftastic unstable-future.sourcegit ];
      };

      dev.vcs.batch.commands = {
        trim = [ "${pkgs.gitAndTools.git-trim}/bin/git-trim --delete=merged-local" ];
      };

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.difftastic # TODO: review package perks
      ];
      ide.emacs.core.config = ''
        (use-package difftastic
          ;; :demand t
          :bind (:map magit-blame-read-only-mode-map
                 ("D" . difftastic-magit-show)
                 ("S" . difftastic-magit-show))
          :config
          (eval-after-load 'magit-diff
            '(transient-append-suffix 'magit-diff '(-1 -1)
               [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
                ("S" "Difftastic show" difftastic-magit-show)])))
      '';
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        git = {
          matches = [
            {
              trigger = ":gmrde";
              replace = "mr direnv";
            }
            {
              trigger = ":gmrt";
              replace = "mr trim";
            }
            {
              trigger = ":gcob";
              replace = "git checkout `git branch --format='%(refname:short)' | fzf`";
            }
            {
              trigger = ":gstexp";
              replace = "git stash show -p `git stash list | fzf | cut -d: -f1` > {{exportbasename.value}}.patch";
              vars = [
                {
                  name = "exportbasename";
                  type = "form";
                  params = { layout = "export to: [[value]]"; };
                }
              ];
            }
            {
              trigger = ":gstsh";
              replace = "git stash show -p `git stash list | fzf | cut -d: -f1`";
            }
            {
              trigger = ":precall";
              replace = "pre-commit run --all-files";
            }
            {
              trigger = ":gpruna";
              replace = "git prune-remote; git prune-local";
            }
            {
              trigger = ":gitsc";
              replace = "git config --list --show-origin --show-scope";
            }
            {
              trigger = ":glcont";
              replace = "git log --pretty=oneline --pickaxe-regex -S$|$";
            }
            {
              trigger = ":gpcont";
              replace = "git log -p --all -S '$|$'";
            }
            {
              trigger = ":gldiff";
              replace = "git log --pretty=oneline --pickaxe-all -G$|$";
            }
            {
              trigger = ":gpdiff";
              replace = "git log -p --all -G '$|$'";
            }
            {
              trigger = ":bdiff";
              replace = "git diff ${config.dev.git.autofetch.mainBranchName} $|$ > ../master-${config.dev.git.autofetch.mainBranchName}.patch";
            }
            {
              trigger = ":tbcont";
              replace = "git log --branches -S'$|$' --oneline | awk '{print $1}' | xargs git branch -a --contains";
            }
            {
              trigger = ":trec";
              replace = "git log -S$|$ --since=HEAD~50 --until=HEAD";
            }
            {
              trigger = ":ghsf";
              replace = "path:**/$|$";
            }
            {
              trigger = ":greb";
              replace = "git rebase `git rev-parse --abbrev-ref --symbolic-full-name '@{u}'`";
            }
            {
              trigger = ":gcs";
              replace = "git show {{clipboard}}";
              vars = [
                {
                  name = "clipboard";
                  type = "clipboard";
                }
              ];
            }
          ];
        };
      };
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          # NOTE: expansions deps
          fzf
          git
          mr
          pre-commit
        ];
      };
    })
  ];
}
