{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.git.misc;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  yaml = pkgs.formats.yaml { };
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
      nixpkgs.config.packageOverrides = _: {
        gittags = mkPythonScriptWithDeps pkgs "gittags"
          (with pkgs; [ python3Packages.pyfzf nurpkgs.pystdlib python3Packages.pygit2 python3Packages.redis ])
          (builtins.readFile ./scripts/gittags.py);
      };

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ gitleaks gitnuro ];
      };

      dev.vcs.batch.commands = {
        synctags = [ "${pkgs.gittags}/bin/gittags --sync" ];
        usynctags = [ "${pkgs.gittags}/bin/gittags --sync --remote ${cfg.defaultUpstreamRemote}" ];
        trim = [ "${pkgs.gitAndTools.git-trim}/bin/git-trim --delete=merged-local" ];
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          # NOTE: expansions deps
          fzf
          git
          mr
          pre-commit
        ];
        xdg.configFile."espanso/match/git.yml".source = yaml.generate "espanso-git.yml"
          {
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
          } // optionalAttrs (config.shell.tmux.enable) {
          filter_title = "\".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*\"";
        };
      };
    })
  ];
}
