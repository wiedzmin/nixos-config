{ config, lib, pkgs, ... }:
# with import ../../pkgs/util.nix { inherit config lib; };
with lib;

let
    cfg = config.dev.git;
    git_lib = pkgs.writeShellScriptBin "git_lib" ''
      WIP_RE=wip

      execute_hook_items() {
        hook=$1
        hooks_basedir=$(pwd)/${cfg.hooks.dirName}

        if [ -z $hook ]; then
          echo "no hook provided, exiting"
          exit 1
        fi

        if [ ! -d "$hooks_basedir" ]; then
          # repo hooks were not initialized, simply exit with success
          exit 0
        fi

        IS_CLEAN=true
        TARGET_DIR="$hooks_basedir/$hook"

        shopt -s execfail

        for TARGET_PATH in $(${pkgs.fd}/bin/fd . -L --max-depth 1 --type f $TARGET_DIR | ${pkgs.coreutils}/bin/sort)
        do
          if [ -x "$TARGET_PATH" ]; then # Run as an executable file
            "$TARGET_PATH" "$@"
          elif [ -f "$TARGET_PATH" ]; then  # Run as a Shell script
            ${pkgs.bash}/bin/bash "$TARGET_PATH" "$@"
          fi
          exitcode=$?
          if (( exitcode != 0 )); then
            IS_CLEAN=false
            ${if cfg.hooks.shortCircuit then "return $exitcode" else ""}
          fi
        done
        if [[ "$IS_CLEAN" == "false" ]]; then
          return 1;
        fi
        return 0;
      }

      check_for_wip() {
        RESULTS=$(${pkgs.git}/bin/git shortlog "@{u}.." | ${pkgs.gnugrep}/bin/grep -w $WIP_RE);
        if [[ ! -z "$RESULTS" ]]; then
          echo "Found commits with stop snippets:"
          echo "$RESULTS"
          return 1;
        fi
        return 0;
      }

      check_for_secrets() { # https://github.com/Luis-Hebendanz/nix-configs/blob/master/git.nix#L25
        RESULTS=$(${pkgs.gitAndTools.git-secrets}/bin/git-secrets --scan --cached 2>&1)
        if [[ -z "$RESULTS" ]]; then
          return 0;
        fi
        echo "Found secret snippets:"
        echo "$RESULTS"
        return 1;
      }

      check_org_delete_treshold() {
        CURRENT_REV=`${pkgs.git}/bin/git rev-parse HEAD`
        PREVIOUS_REV=`${pkgs.git}/bin/git rev-parse HEAD^1`

        OUTFILE="${cfg.org.warningsFile}"
        THRESHOLD=250

        MESSAGE="** commit ''${CURRENT_REV} deleted more than ''${THRESHOLD} lines in a file!"

        DETAILS="#+BEGIN_SRC sh :results output
        cd ${builtins.dirOf cfg.org.warningsFile}
        echo \"commit ''${CURRENT_REV}\"
        ${pkgs.git}/bin/git diff --stat \"''${PREVIOUS_REV}\" \"''${CURRENT_REV}\"
        #+END_SRC"

        ${pkgs.git}/bin/git diff --numstat "''${PREVIOUS_REV}" "''${CURRENT_REV}" | \
          cut -f 2 | \
          while read line
            do test "$line" -gt "''${THRESHOLD}" && \
              echo "''${MESSAGE}\n<`date '+%Y-%m-%d %H:%M'` +1d>\n\n''${DETAILS}\n" >> \
              "''${OUTFILE}"; \
            done
      }
    '';
    bitbucket_team_contributor_repos = pkgs.writeShellScriptBin "bitbucket_team_contributor_repos" ''
      PASS_PATH=$1
      if [ -z "PASS_PATH" ]; then
        echo "No credentials provided"
        exit 1
      fi
      TEAM=$2
      PROJECTS_EXCLUDE=$3
      CREDENTIALS=$(${pkgs.pass}/bin/pass $PASS_PATH | ${pkgs.coreutils}/bin/tr '\n' ' ' | ${pkgs.gawk}/bin/awk '{print $3 ":" $1}')
      RESULT=$(${pkgs.curl}/bin/curl -s -u \
             $CREDENTIALS "https://api.bitbucket.org/2.0/repositories?role=contributor&pagelen=200" | \
             ${pkgs.jq}/bin/jq -r '.values[] | select(.project.name != null) | "\(.links.clone[0].href)~\(.project.name)"')
      if [[ ! -z $TEAM ]]; then
        RESULT=$(printf "%s\n" $RESULT | ${pkgs.gnugrep}/bin/grep $TEAM)
      fi
      if [[ ! -z $PROJECTS_EXCLUDE ]]; then
        GREP_CLAUSES=$(echo $PROJECTS_EXCLUDE | ${pkgs.gnused}/bin/sed "s/,/\|/g")
        RESULT=$(printf "%s\n" $RESULT | ${pkgs.gnugrep}/bin/grep -i -v -E $GREP_CLAUSES)
      fi
      for REPO in $RESULT; do
        echo $REPO | ${pkgs.coreutils}/bin/cut -f1 -d~
      done
    '';
    git_remote_diff = pkgs.writeShellScriptBin "git_remote_diff" ''
      GIT_REPO=''${1:-'.'}
      cd $GIT_REPO
      if [ -z "$(${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null)" ]; then
        echo "Not a git repo"
        exit 1
      fi
      UPSTREAM=''${2:-'@{u}'}
      LOCAL=$(${pkgs.git}/bin/git rev-parse @)
      REMOTE=$(${pkgs.git}/bin/git rev-parse "$UPSTREAM")
      BASE=$(${pkgs.git}/bin/git merge-base @ "$UPSTREAM")

      if [ "$LOCAL" == "$REMOTE" ]; then
        echo ''${UPTODATE_MESSAGE:-"Up-to-date"}
      elif [ "$LOCAL" == "$BASE" ]; then
        echo ''${NEEDFETCH_MESSAGE:-"Need to fetch"}
      elif [ "$REMOTE" == "$BASE" ]; then
        echo ''${NEEDPUSH_MESSAGE:-"Need to push"}
      else
        echo ''${DIVERGED_MESSAGE:-"Diverged"}
      fi
    '';
in {
  options = {
    dev.git = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git VCS infrastructure.";
      };
      enableNixosConfigWipChecks = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable pre commit hook for NixOS config
          repo, that checks for work-in-progress code.
        '';
      };
      defaultUpstreamRemote = mkOption {
        type = types.str;
        default = "upstream";
        description = "Name of upstream repo remote.";
      };
      pager.diff-so-fancy.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable diff-so-fancy pager.";
      };
      pager.delta.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable delta pager.";
      };
      workspaceRoot = mkOption {
        type = types.str;
        default = "";
        description = "Root directory for all repositories.";
      };
      signing.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable cryptographic objects signing.";
      };
      hooks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable custom hooks.";
      };
      hooks.shortCircuit = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to short-circuit hooks chain for particular repo.";
      };
      hooks.dirName = mkOption {
        type = types.str;
        default = ".hooks";
        description = "Hooks storage dir name for particular repo.";
      };
      assets.dirName = mkOption {
        type = types.str;
        default = "git-assets";
        description = "Assets(templates, etc.) home subdir.";
      };
      myrepos.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Myrepos setup.";
      };
      myrepos.subconfigs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Myrepos subconfigs to include.";
      };
      ghq.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable ghq tooling.";
      };
      ghq.importCommands = mkOption {
        type = types.attrs;
        default = {};
        description = "Custom import commands for ghq.";
      };
      github.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Github connectivity.";
      };
      github.user = mkOption {
        type = types.str;
        default = "";
        description = "Github user ID.";
      };
      fetchUpdates.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable batch updates fetching.";
      };
      fetchUpdates.when = mkOption {
        type = types.str;
        default = "";
        description = "When to fetch updates (on calendar).";
      };
      pushUpdates.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable batch updates pushing.";
      };
      pushUpdates.when = mkOption {
        type = types.str;
        default = "";
        description = "When to push updates (on calendar).";
      };
      saveWip.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable work-in-progress saving.";
      };
      saveWip.when = mkOption {
        type = types.str;
        default = "";
        description = "When to save work-in-progress (on calendar).";
      };
      org.warningsFile = mkOption {
        type = types.str;
        default = "$HOME/warnings.org";
        description = "Org-mode file to place accidental deletes diff.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.attributes.mainUser.fullName != "" && config.attributes.mainUser.email != "";
          message = "git: Must provide authoring identity.";
        }
        {
          assertion = cfg.github.enable && cfg.github.user != "";
          message = "git: Must provide github ID when enabling Github connectivity.";
        }
        {
          assertion = let
            p = cfg.pager.diff-so-fancy.enable;
            q = cfg.pager.delta.enable;
          in (p || q) && !(p && q) || (!p && !q);
          message = "git: Must choose either none or exactly one (`diff-so-fancy` or `delta`) custom pager.";
        }
      ] ++ lib.optionals (cfg.signing.enable) [{
        assertion = config.attributes.mainUser.gpgKeyID != "";
        message = "git: Must provide GPG key ID when objects signing is enabled.";
      }];

      home-manager.users."${config.attributes.mainUser.name}" = {
        home.file = {
          # TODO: tear apart to respective modules later (e.g. emacs, vim, python, etc.)
          "${cfg.assets.dirName}/.gitignore".text = ''
            # emacs
            *.elc
            .dir-locals.el

            # various
            *.out
            *.swp
            .mypy_cache/*

            ${cfg.hooks.dirName}
          '';
        };
        # TODO: conditionalize/parameterize
        xdg.configFile."pass-git-helper/git-pass-mapping.ini".text = ''
          [github.com*]
          target=${config.attributes.mainUser.name}/webservices/social/programming/github.com

          [bitbucket.org*]
          target=${config.attributes.mainUser.name}/webservices/social/programming/bitbucket.com
        '';
        programs.git = {
          enable = true;
          userName = config.attributes.mainUser.fullName;
          userEmail = config.attributes.mainUser.email;
          signing = {
            key = config.attributes.mainUser.gpgKeyID;
            signByDefault = true;
          };
          extraConfig = {
            "rebase" = {
              autoSquash = true;
              autoStash = true;
            };
            "core" = {
              autocrlf = false;
              excludesfile = "/home/${config.attributes.mainUser.name}/${cfg.assets.dirName}/.gitignore";
              quotepath = false;
              askPass = "";
            } // lib.optionalAttrs (cfg.hooks.enable) {
              hooksPath = "/home/${config.attributes.mainUser.name}/${cfg.assets.dirName}/templates/hooks";
            };
            "credential" = { helper = "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper"; };
            "diff" = { algorithm = "patience"; };
            "init" = { templatedir = "/home/${config.attributes.mainUser.name}/${cfg.assets.dirName}/templates"; };
            "clone" = { templatedir = "/home/${config.attributes.mainUser.name}/${cfg.assets.dirName}/templates"; };
            "push" = { default = "current"; };
          } // lib.optionalAttrs (cfg.github.enable) {
            "github" = { user = cfg.github.user; };
          } // lib.optionalAttrs (cfg.pager.diff-so-fancy.enable) {
            "pager" = {
              diff = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
              show = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
              log = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
            };
          } // lib.optionalAttrs (cfg.pager.delta.enable) {
            "pager" = {
              diff = ''${pkgs.gitAndTools.delta}/bin/delta --plus-color="#012800" \
                                                           --minus-color="#340001" \
                                                           --highlight-removed \
                                                           --theme="zenburn"
                                                           '';
              show = ''${pkgs.gitAndTools.delta}/bin/delta --plus-color="#012800" \
                                                           --minus-color="#340001" \
                                                           --highlight-removed \
                                                           --theme="zenburn"
                                                           '';
              log = ''${pkgs.gitAndTools.delta}/bin/delta --plus-color="#012800" \
                                                          --minus-color="#340001" \
                                                          --highlight-removed \
                                                          --theme="zenburn"
                                                          '';
            };
          };

          aliases = {
            bl = "branch -l";
            merged = "branch --merged master";
            nomerged = "branch --no-merged master";

            undo = "reset HEAD~1";
            hundo = "reset --hard HEAD~1";

            who = "shortlog -n -s --no-merges";
            sl = "log --name-only --oneline";

            # TODO: think of using --patience
            df = "diff --patch-with-stat --color --color-words --abbrev";
            last = "diff --stat=150,120 HEAD^ HEAD";

            pf = "format-patch -1 --no-prefix -s -p FETCH_HEAD";

            remotes = "remote -v";
            slice = "clone --depth 1";
          };
        };
        programs.zsh.shellAliases = {
          git = "${pkgs.gitAndTools.hub}/bin/hub";
        };
      };
      environment.systemPackages = with pkgs; [
        git-crecord
        git-sizer
        gitAndTools.git-absorb # TODO: review abilities and maybe use in some automation
        gitAndTools.git-extras
        gitAndTools.git-octopus
        gitAndTools.pass-git-helper
        gitAndTools.stgit
        gitAndTools.topGit # https://github.com/mackyle/topgit
        git_remote_diff
        gitstats
        proposed.gitAndTools.git-quick-stats
        python2Packages.git-sweep # FIXME: adapt to py3
      ];
    })
    (mkIf (cfg.enable && cfg.ghq.enable) {
      assertions = [
        {
          assertion = cfg.workspaceRoot != "";
          message = "git: Must provide workspace root directory for ghq tool.";
        }
      ];

      environment.systemPackages = with pkgs; [
        bitbucket_team_contributor_repos
        gitAndTools.ghq
      ];
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.zsh.shellAliases = {
          gg = "${pkgs.gitAndTools.ghq}/bin/ghq get";
        };
        programs.git.extraConfig = {
          "ghq" = { root = cfg.workspaceRoot; };
          "ghq \"import\"" = cfg.ghq.importCommands;
        };
      };
    })
    (mkIf (cfg.enable && cfg.enableNixosConfigWipChecks) {
      environment.etc."nixos/${cfg.hooks.dirName}/pre-push/stop-wip" = {
        mode = "0644";
        user = "${config.attributes.mainUser.name}";
        group = "users";
        text = ''
          . ${git_lib}/bin/git_lib

          check_for_wip
          exit $?
        '';
      };
    })
    (mkIf (cfg.enable && cfg.myrepos.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.file = {
          ".mrtrust".text = builtins.concatStringsSep "\n" cfg.myrepos.subconfigs;
          # TODO: review https://github.com/RichiH/myrepos/blob/master/mrconfig.complex
          ".mrconfig".text = ''
            [DEFAULT]
            update =
              ${pkgs.git}/bin/git fetch origin
              ${pkgs.git}/bin/git rebase origin/$(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD)
            savewip =
              if [[ ! -z $(${pkgs.git}/bin/git status --porcelain) ]]; then
                ${pkgs.git}/bin/git add .
                ${pkgs.git}/bin/git commit -m "WIP $(${pkgs.coreutils}/bin/date -R)"
              else
                return 0
              fi
            push =
              if [[ $(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD) =~ master ]]; then
                echo master is active, skipping...
              else
                ${pkgs.git}/bin/git push origin $(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD)
              fi
            usync =
              if [[ ! -z $(${pkgs.git}/bin/git remote | grep ${cfg.defaultUpstreamRemote}) ]]; then
                ${pkgs.git}/bin/git fetch ${cfg.defaultUpstreamRemote}
                current_branch=$(${pkgs.git}/bin/git branch | ${pkgs.gnugrep}/bin/grep \* | ${pkgs.coreutils}/bin/cut -d ' ' -f2)
                ${pkgs.git}/bin/git merge ${cfg.defaultUpstreamRemote}/$current_branch
              else
                echo No upstream defined, skipping...
                return 0
              fi

            ${lib.concatMapStrings (config: ''
              include = cat ${config}
            '') cfg.myrepos.subconfigs}
          '';
        };
      };
    })
    (mkIf (cfg.enable && cfg.hooks.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.file = {
          "${cfg.assets.dirName}/templates/hooks/pre-push" = {
            executable = true;
            text = ''
              #!${pkgs.bash}/bin/bash
              . ${git_lib}/bin/git_lib
              execute_hook_items pre-push
              exit $?;
            '';
          };
          "${cfg.assets.dirName}/templates/hooks/pre-commit" = {
            executable = true;
            text = ''
              #!${pkgs.bash}/bin/bash
              . ${git_lib}/bin/git_lib
              execute_hook_items pre-commit
              exit $?;
            '';
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.fetchUpdates.enable) {
      assertions = [
        {
          assertion = cfg.myrepos.enable;
          message = "git: automatic updates fetching requires myrepos setup to be enabled.";
        }
      ];

      systemd.user.services."git-fetch-updates" = {
        description = "Fetch updates from registered git upstream(s)";
        path = pathPkgs;
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.mr}/bin/mr update";
          WorkingDirectory = "/home/${config.attributes.mainUser.name}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
    })
    (mkIf (cfg.enable && cfg.fetchUpdates.enable && cfg.fetchUpdates.when != "") {
      assertions = [
        {
          assertion = cfg.myrepos.enable;
          message = "git: automatic updates fetching requires myrepos setup to be enabled.";
        }
      ];

      systemd.user.timers."git-fetch-updates" = {
        description = "Fetch updates from registered git upstream(s)";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfg.fetchUpdates.when;
        };
      };
    })
    (mkIf (cfg.enable && cfg.pushUpdates.enable) {
      assertions = [
        {
          assertion = cfg.myrepos.enable;
          message = "git: automatic updates pushing requires myrepos setup to be enabled.";
        }
      ];

      systemd.services."git-push-updates" = {
        description = "Push updates to registered git upstream(s)";
        path = pathPkgs;
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.mr}/bin/mr push";
          WorkingDirectory = "/home/${config.attributes.mainUser.name}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
    })
    (mkIf (cfg.enable && cfg.pushUpdates.enable && cfg.pushUpdates.when != "") {
      assertions = [
        {
          assertion = cfg.myrepos.enable;
          message = "git: automatic updates pushing requires myrepos setup to be enabled.";
        }
      ];

      systemd.timers."git-push-updates" = {
        description = "Push updates to registered git upstream(s)";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfg.pushUpdates.when;
        };
      };
    })
    (mkIf (cfg.enable && cfg.saveWip.enable) {
      assertions = [
        {
          assertion = cfg.myrepos.enable;
          message = "git: automatic WIP saving requires myrepos setup to be enabled.";
        }
      ];

      systemd.user.services."git-save-wip" = {
        description = "Save work-in-progress in registered git repo(s)";
        path = pathPkgs;
        serviceConfig = {
          Type = "oneshot";
          ExecStart = ''
            ${pkgs.bash}/bin/bash -c "[[ $(${pkgs.xprintidle-ng}/bin/xprintidle-ng) -ge $((3600*1000)) ]] && ${pkgs.mr}/bin/mr savewip"''; # TODO: only when not on master
          WorkingDirectory = "/home/${config.attributes.mainUser.name}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
    })
    (mkIf (cfg.enable && cfg.saveWip.enable && cfg.saveWip.when != "") {
      assertions = [
        {
          assertion = cfg.myrepos.enable;
          message = "git: automatic WIP saving requires myrepos setup to be enabled.";
        }
      ];

      systemd.user.timers."git-save-wip" = {
        description = "Save work-in-progress in registered git repo(s)";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfg.saveWip.when;
        };
      };
    })
  ];
}
