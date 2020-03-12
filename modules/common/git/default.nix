{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.dev.git;
  # TODO: make custom script with base of https://github.com/arc90/git-sweep
in {
  options = {
    custom.dev.git = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git VCS infrastructure.";
      };
      enableNixosConfigGoodies = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable pre commit hook for NixOS config
          repo, that checks for work-in-progress code.
          ...and gpg-aware .gitattributes.
        '';
      };
      defaultUpstreamRemote = mkOption {
        type = types.str;
        default = "upstream";
        description = "Name of upstream repo remote.";
      };
      urlSubstitutes = mkOption {
        type = types.attrsOf types.attrs;
        default = { };
        description = "Config' `insteadOf` entries mapping.";
      };
      idletime.stgit = mkOption {
        type = types.int;
        default = 3600;
        description = "Seconds of X idle time to start stgit actions.";
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
        default = builtins.attrValues
          (builtins.mapAttrs (_: value: value + "/.mrconfig") config.custom.dev.workspaceRoots);
        description = "Myrepos subconfigs to include.";
      };
      ghq.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable ghq tooling.";
      };
      ghq.importCommands = mkOption {
        type = types.attrs;
        default = { };
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
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs git-related setup.";
      };
      emacs.extraConfig = mkOption {
        type = types.lines;
        default = '''';
        description = "Extra settings to be added to Emacs config.";
      };
      staging.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable staging settings for git.";
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

      nixpkgs.config.packageOverrides = _: rec {
        gitlib = pkgs.writeShellScriptBin "gitlib" (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./gitlib.sh; })));
        git-save-wip = pkgs.writeShellScriptBin "git-save-wip" (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./git-save-wip.sh; })));
      };

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
            "diff" = {
              algorithm = "patience";
              gpg = { textconv = "${pkgs.gnupg}/bin/gpg2 --no-tty --decrypt"; };
            };
            "init" = { templatedir = "/home/${config.attributes.mainUser.name}/${cfg.assets.dirName}/templates"; };
            "clone" = { templatedir = "/home/${config.attributes.mainUser.name}/${cfg.assets.dirName}/templates"; };
            "push" = { default = "current"; };
            "absorb" = { maxstack = 75; };
          } // lib.optionalAttrs (cfg.github.enable) { "github" = { user = cfg.github.user; }; }
            // lib.optionalAttrs (cfg.pager.diff-so-fancy.enable) {
              "pager" = {
                diff = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
                show = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
                log = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
              };
            } // lib.optionalAttrs (cfg.pager.delta.enable) {
              "pager" = {
                diff = ''
                  ${pkgs.gitAndTools.delta}/bin/delta --dark --plus-color="#34ad3a" --minus-color="#ad3436" --highlight-removed --theme="zenburn"'';
                show = ''
                  ${pkgs.gitAndTools.delta}/bin/delta --dark --plus-color="#34ad3a" --minus-color="#ad3436" --highlight-removed --theme="zenburn"'';
                log = ''
                  ${pkgs.gitAndTools.delta}/bin/delta --dark --plus-color="#34ad3a" --minus-color="#ad3436" --highlight-removed --theme="zenburn"'';
              };
            } // lib.optionalAttrs (cfg.urlSubstitutes != { }) cfg.urlSubstitutes;

          aliases = {
            bl = "branch -l";
            merged = "branch --merged master";
            nomerged = "branch --no-merged master";

            st = "status --short --branch";

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
        programs.zsh.shellAliases = { git = "${pkgs.gitAndTools.hub}/bin/hub"; };
      };
      environment.systemPackages = with pkgs;
        [
          git-crecord
          git-sizer
          gitAndTools.git-absorb # TODO: review abilities and maybe use in some automation
          gitAndTools.git-crypt
          gitAndTools.git-extras
          gitAndTools.git-octopus
          gitAndTools.lab
          gitAndTools.pass-git-helper
          gitAndTools.stgit
          gitAndTools.thicket
          gitstats
          git-quick-stats

          git-save-wip
        ] ++ lib.optionals (cfg.staging.enable) [ onefetch overcommit gitAndTools.git-machete gitAndTools.git-my ];
    })
    (mkIf (cfg.enable && cfg.ghq.enable) {
      assertions = [{
        assertion = config.custom.dev.workspaceRoots.global != "";
        message = "git: Must provide workspace root directory for ghq tool.";
      }];

      environment.systemPackages = with pkgs; [ gitAndTools.ghq ];
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.zsh.shellAliases = {
          gg = "${pkgs.gitAndTools.ghq}/bin/ghq get";
        } // lib.optionalAttrs (config.custom.navigation.misc.enable) {
          pgg = "${pkgs.pueue}/bin/pueue add -- ${pkgs.gitAndTools.ghq}/bin/ghq get";
        };
        programs.git.extraConfig = { "ghq" = { root = config.custom.dev.workspaceRoots.global; }; };
      };
    })
    (mkIf (cfg.enable && cfg.enableNixosConfigGoodies) {
      # FIXME: provide recursive permissions setting
      environment.etc."nixos/.gitattributes".text = ''
        *.gpg filter=gpg diff=gpg
        **/secrets/** filter=git-crypt diff=git-crypt
      '';
      environment.etc."nixos/${cfg.hooks.dirName}/pre-push/stop-wip" = {
        mode = "0644";
        user = config.attributes.mainUser.name;
        group = "users";
        text = ''
          . ${pkgs.gitlib}/bin/gitlib

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
          # TODO: consider stage and commit all WIP before pushing
          ".mrconfig".text = ''
            [DEFAULT]
            lib =
              on_master() {
                test $(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD) = "master"
              }
            update =
              FORCE_STG=yes ${pkgs.git-save-wip}/bin/git-save-wip .
              ${pkgs.git}/bin/git fetch origin
              ${pkgs.git}/bin/git rebase origin/$(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD)
            savewip =
              ${pkgs.git-save-wip}/bin/git-save-wip .
            push =
              if ! on_master; then
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
            synctags =
              ${pkgs.git}/bin/git fetch origin --tags
              ${pkgs.git}/bin/git push origin --tags
            usynctags =
              if [[ ! -z $(${pkgs.git}/bin/git remote | grep ${cfg.defaultUpstreamRemote}) ]]; then
                ${pkgs.git}/bin/git fetch ${cfg.defaultUpstreamRemote} --tags
                ${pkgs.git}/bin/git push ${cfg.defaultUpstreamRemote} --tags
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
              . ${pkgs.gitlib}/bin/gitlib
              execute_hook_items pre-push
              exit $?;
            '';
          };
          "${cfg.assets.dirName}/templates/hooks/pre-commit" = {
            executable = true;
            text = ''
              #!${pkgs.bash}/bin/bash
              . ${pkgs.gitlib}/bin/gitlib
              execute_hook_items pre-commit
              exit $?;
            '';
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.fetchUpdates.enable) {
      assertions = [{
        assertion = cfg.myrepos.enable;
        message = "git: automatic updates fetching requires myrepos setup to be enabled.";
      }];

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
      assertions = [{
        assertion = cfg.myrepos.enable;
        message = "git: automatic updates fetching requires myrepos setup to be enabled.";
      }];

      systemd.user.timers."git-fetch-updates" = {
        description = "Fetch updates from registered git upstream(s)";
        wantedBy = [ "timers.target" ];
        timerConfig = { OnCalendar = cfg.fetchUpdates.when; };
      };
    })
    (mkIf (cfg.enable && cfg.pushUpdates.enable) {
      assertions = [{
        assertion = cfg.myrepos.enable;
        message = "git: automatic updates pushing requires myrepos setup to be enabled.";
      }];

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
      assertions = [{
        assertion = cfg.myrepos.enable;
        message = "git: automatic updates pushing requires myrepos setup to be enabled.";
      }];

      systemd.timers."git-push-updates" = {
        description = "Push updates to registered git upstream(s)";
        wantedBy = [ "timers.target" ];
        timerConfig = { OnCalendar = cfg.pushUpdates.when; };
      };
    })
    (mkIf (cfg.enable && cfg.saveWip.enable) {
      assertions = [{
        assertion = cfg.myrepos.enable;
        message = "git: automatic WIP saving requires myrepos setup to be enabled.";
      }];

      systemd.user.services."git-save-wip" = {
        description = "Save work-in-progress in registered git repo(s)";
        path = pathPkgs;
        serviceConfig = {
          Type = "oneshot";
          ExecStart = ''
            ${pkgs.bash}/bin/bash -c "${pkgs.mr}/bin/mr savewip"'';
          WorkingDirectory = "/home/${config.attributes.mainUser.name}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
    })
    (mkIf (cfg.enable && cfg.saveWip.enable && cfg.saveWip.when != "") {
      assertions = [{
        assertion = cfg.myrepos.enable;
        message = "git: automatic WIP saving requires myrepos setup to be enabled.";
      }];

      systemd.user.timers."git-save-wip" = {
        description = "Save work-in-progress in registered git repo(s)";
        wantedBy = [ "timers.target" ];
        timerConfig = { OnCalendar = cfg.saveWip.when; };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs:
          [
            epkgs.dired-git-info
            epkgs.git-link
            epkgs.git-msg-prefix
            epkgs.git-timemachine
            epkgs.git-walktree
            epkgs.magit
            epkgs.magit-filenotify
            epkgs.magit-popup # *
            epkgs.magit-todos
          ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./git.el; })) + "\n" +
          cfg.emacs.extraConfig;
    })
  ];
}

# * it seems some magit-dependent packages yet depend on magit-popup in some path, so we introduced
#   this explicit dependency and will keep it until transition to "transient" library is fully done
#   by all affected packages. (or some other root cause of "magit-popup"" will pop up)

# think of automation (maybe using existing tools) for providing branch name for Jira-related (and
# maybe not only) projects
