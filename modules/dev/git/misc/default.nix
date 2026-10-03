{ config, lib, pkgs, ... }:
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
      worktreesRoot = mkOption {
        type = types.str;
        default = homePrefix user "workspace/worktrees";
        description = "Path to wotktrees root";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ difftastic ];
        programs.lazyworktree = {
          enable = true;
          settings = {
            worktree_dir = cfg.worktreesRoot;
            sort_mode = "switched";
            layout = "default";
            auto_refresh = true;
            ci_auto_refresh = false;
            refresh_interval = 10;
            disable_pr = false;
            icon_set = "nerd-font-v3";
            search_auto_select = false;
            fuzzy_finder_input = false;
            palette_mru = true;
            palette_mru_limit = 5;
          };
          shellWrapperName = "wt";
        };
        programs.television = {
          settings.shell_integration.channel_triggers = {
            "git-branches" = [
              "git checkout"
            ];
          };
          channels = {
            git-branches = {
              metadata = {
                name = "git-branches";
                description = "A channel to select branch in current git repo";
                requirements = [ "git" ];
              };
              source = {
                command = "git branch --format='%(refname:short)'";
              };
            };
            git-stashes = {
              metadata = {
                name = "git-stashes";
                description = "A channel to select stash entry in current git repo";
                requirements = [ "git" ];
              };
              source = {
                command = "git stash list --format='%gd %h %f'";
                output = "{split: :0}";
              };
              preview = {
                command = "git stash show -p {split: :0}";
              };
              ui = {
                preview_panel = {
                  header = "{split: :2}";
                  footer = "{split: :1}";
                };
              };
              keybindings = {
                "ctrl-alt-e" = "actions:export";
              };
              actions = {
                "export" = {
                  description = "Export stash to .patch";
                  command = "git stash show -p {split: :0} > {split: :1}-{split: :2}.patch";
                };
              };
            };
          };
        };
        home.activation.ensureWorktreesRoot = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${cfg.worktreesRoot}";
        };
      };

      dev.vcs.batch.commands = {
        trim = [ "${pkgs.git-trim}/bin/git-trim --delete=merged-local" ];
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
      navigation.bookmarks.entries = {
        worktrees-root = {
          desc = "Git worktrees global root";
          path = cfg.worktreesRoot;
        };
      };
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
              trigger = ":gco";
              replace = "git checkout $|$";
            }
            {
              trigger = ":gst";
              replace = "git stash show -p $|$";
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
