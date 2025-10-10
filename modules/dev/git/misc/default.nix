{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# <[github backup]> - <consult-ripgrep "/home/alex3rd/workspace/repos/github.com/NixOS/nixpkgs/" "github backup description">

# nsp>gitleaks|gitnuro|mgitstatus|sourcegit
# nsp>gitleaks npkg#gitleaks
# nsp>gitnuro npkg#gitnuro
# nsp>mgitstatus npkg#mgitstatus
# nsp>sourcegit npkg#sourcegit

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
        home.packages = with pkgs; [ difftastic ];
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
              replace = "mr direnv"; # nsp>mr npkg#mr
            }
            {
              trigger = ":gmrt";
              replace = "mr trim"; # nsp>mr npkg#mr
            }
            {
              trigger = ":gcob";
              replace = "git checkout `git branch --format='%(refname:short)' | fzf`"; # nsp>git|fzf
            }
            {
              trigger = ":gstexp";
              replace = "git stash show -p `git stash list | fzf | cut -d: -f1` > {{exportbasename.value}}.patch"; # nsp>git|fzf
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
              replace = "git stash show -p `git stash list | fzf | cut -d: -f1`"; # nsp>git|fzf
            }
            {
              trigger = ":precall";
              replace = "pre-commit run --all-files"; # nsp>pre-commit npkg#pre-commit
            }
            {
              trigger = ":gpruna";
              replace = "git prune-remote; git prune-local"; # nsp>git npkg#git
            }
            {
              trigger = ":gitsc";
              replace = "git config --list --show-origin --show-scope"; # nsp>git npkg#git
            }
            {
              trigger = ":glcont";
              replace = "git log --pretty=oneline --pickaxe-regex -S$|$"; # nsp>git npkg#git
            }
            {
              trigger = ":gpcont";
              replace = "git log -p --all -S '$|$'"; # nsp>git npkg#git
            }
            {
              trigger = ":gldiff";
              replace = "git log --pretty=oneline --pickaxe-all -G$|$"; # nsp>git npkg#git
            }
            {
              trigger = ":gpdiff";
              replace = "git log -p --all -G '$|$'"; # nsp>git npkg#git
            }
            {
              trigger = ":bdiff";
              replace = "git diff ${config.dev.git.autofetch.mainBranchName} $|$ > ../master-${config.dev.git.autofetch.mainBranchName}.patch"; # nsp>git npkg#git
            }
            {
              trigger = ":tbcont";
              replace = "git log --branches -S'$|$' --oneline | awk '{print $1}' | xargs git branch -a --contains"; # nsp>git npkg#git
            }
            {
              trigger = ":trec";
              replace = "git log -S$|$ --since=HEAD~50 --until=HEAD"; # nsp>git npkg#git
            }
            {
              trigger = ":ghsf";
              replace = "path:**/$|$";
            }
            {
              trigger = ":greb";
              replace = "git rebase `git rev-parse --abbrev-ref --symbolic-full-name '@{u}'`"; # nsp>git npkg#git
            }
            {
              trigger = ":gcs";
              replace = "git show {{clipboard}}"; # nsp>git npkg#git
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
    })
  ];
}
