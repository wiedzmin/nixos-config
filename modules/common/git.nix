{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.dev.git;
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
  # TODO: make custom script with base of https://github.com/arc90/git-sweep
  emacsGitSetup = ''
    ${lib.optionalString (cfg.strictRemoteBrowsing) ''
    (use-package browse-at-remote
      :ensure t
      :after link-hint
      :bind
      (:map link-hint-keymap
            ("r" . browse-at-remote)
            ("k" . browse-at-remote-kill))
      (:map magit-status-mode-map
            ("o" . browse-at-remote))
      :custom
      (browse-at-remote-prefer-symbolic nil))
    ''}
    ${lib.optionalString (!cfg.strictRemoteBrowsing) ''
    (use-package git-link
      :ensure t
      :after link-hint
      :bind
      (:map link-hint-keymap
            ("r" . git-link)
            ("c" . git-link-commit))
      (:map magit-status-mode-map
            ("o" . git-link))
      :custom
      (git-link-open-in-browser t))
    ''}

    (use-package git-timemachine
      :ensure t
      :bind
      (:map mode-specific-map
            (";" . git-timemachine)))

    (use-package magit
      :ensure t
      :mode (("COMMIT_EDITMSG" . conf-javaprop-mode)
             ("COMMIT" . git-commit-mode))
      :bind
      (:prefix-map custom-magit-map
                   :prefix "C-'"
                   ("B" . magit-branch)
                   ("L" . magit-reflog-current)
                   ("O" . magit-reflog-other)
                   ("R" . magit-rebase)
                   ("S" . magit-stash)
                   ("U" . magit-update-index)
                   ("a" . magit-stage-file)
                   ("b" . magit-blame-addition) ; TODO: add for *-removal
                   ("c" . magit-checkout)
                   ("d" . magit-diff)
                   ("f" . magit-log-buffer-file)
                   ("i" . magit-init)
                   ("l" . magit-log)
                   ("n" . magit-notes-edit)
                   ("r" . magit-reset)
                   ("s" . magit-status)
                   ("t" . magit-tag)
                   ("w" . magit-diff-working-tree))
      (:map magit-status-mode-map
            ("E" . nil)
            ("N" . magit-notes-edit)
            ("q" . custom/magit-kill-buffers))
      (:map dired-mode-map
            ("@" . magit-dired-log))
      :preface
      (defun open-global-repos-list ()
        (interactive)
        (let ((repos-buffer (get-buffer "*Magit Repositories*")))
          (if repos-buffer
              (switch-to-buffer repos-buffer)
            (magit-list-repositories))))
      (defun custom/magit-restore-window-configuration (&optional kill-buffer)
        "Bury or kill the current buffer and restore previous window configuration."
        (let ((winconf magit-previous-window-configuration)
              (buffer (current-buffer))
              (frame (selected-frame)))
          (quit-window kill-buffer (selected-window))
          (when (and winconf (equal frame (window-configuration-frame winconf)))
            (set-window-configuration winconf)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (setq magit-previous-window-configuration nil))))))
      (defun custom/magit-kill-buffers ()
        "Restore window configuration and kill all Magit buffers."
        (interactive)
        (let ((buffers (magit-mode-get-buffers)))
          (magit-restore-window-configuration)
          (mapc #'kill-buffer buffers)))
      :custom
      (magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
      (magit-completing-read-function 'ivy-completing-read)
      (magit-blame-heading-format "%H %-20a %C %s")
      (magit-diff-refine-hunk t)
      (magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1))

    (use-package magit-filenotify
      :ensure t
      :delight (magit-filenotify-mode " FN")
      :hook (magit-status-mode-hook . (lambda ()
                                        (condition-case nil
                                            (magit-filenotify-mode)
                                          (error (magit-filenotify-mode -1))))))

    (use-package git-walktree
      :ensure t
      :after magit
      :bind
      (:map custom-magit-map
            ("o" . git-walktree)))

    (use-package dired-git-info
      :ensure t
      :after dired
      ;; :hook (dired-after-readin-hook . dired-git-info-auto-enable)
      :bind
      (:map dired-mode-map
            (")" . dired-git-info-mode)))

    (use-package git-msg-prefix
      :ensure t
      :bind
      (:map git-commit-mode-map
            ("C-c i" . git-msg-prefix))
      :custom
      (git-msg-prefix-log-flags " --since='1 week ago' ")
      (git-msg-prefix-input-method 'ivy-read))

    (use-package magit-todos
      :ensure t
      :bind
      (:map mode-specific-map
            ("C-d" . ivy-magit-todos))
      :hook
      (magit-status-mode-hook . magit-todos-mode))

    (use-package smerge-mode
      :delight (smerge-mode "∓")
      :bind
      (:map mode-specific-map
            ("g k" . smerge-prev)
            ("g j" . smerge-next))
      :hook (find-file-hooks . (lambda ()
                                 (save-excursion
                                   (goto-char (point-min))
                                   (when (re-search-forward "^<<<<<<< " nil t)
                                     (smerge-mode 1))))))
  '';
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
      strictRemoteBrowsing = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to treat remote git references
          strictly, while opening them in browser.

          If strict references are used, then,
          for example opening repository link will
          fail, if there are uncommited/unpushed
          changes.
        '';
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
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs git-related setup.";
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
            "diff" = {
              algorithm = "patience";
              gpg = {
                textconv = "${pkgs.gnupg}/bin/gpg2 --no-tty --decrypt";
              };
            };
            "init" = { templatedir = "/home/${config.attributes.mainUser.name}/${cfg.assets.dirName}/templates"; };
            "clone" = { templatedir = "/home/${config.attributes.mainUser.name}/${cfg.assets.dirName}/templates"; };
            "push" = { default = "current"; };
            "absorb" = { maxstack = 75; };
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
              diff = ''${pkgs.gitAndTools.delta}/bin/delta --dark --plus-color="#34ad3a" --minus-color="#ad3436" --highlight-removed --theme="zenburn"'';
              show = ''${pkgs.gitAndTools.delta}/bin/delta --dark --plus-color="#34ad3a" --minus-color="#ad3436" --highlight-removed --theme="zenburn"'';
              log = ''${pkgs.gitAndTools.delta}/bin/delta --dark --plus-color="#34ad3a" --minus-color="#ad3436" --highlight-removed --theme="zenburn"'';
            };
          };

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
        gitAndTools.lab
        gitAndTools.pass-git-helper
        gitAndTools.stgit
        gitstats
        git-quick-stats
      ] ++ lib.optionals (config.attributes.staging.enable) [
        onefetch
        overcommit
        gitAndTools.git-machete
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
        gitAndTools.ghq
      ];
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.zsh.shellAliases = {
          gg = "${pkgs.gitAndTools.ghq}/bin/ghq get";
        };
        programs.git.extraConfig = {
          "ghq" = { root = cfg.workspaceRoot; };
        };
      };
    })
    (mkIf (cfg.enable && cfg.enableNixosConfigGoodies) {
      # FIXME: provide recursive permissions setting
      environment.etc."nixos/.gitattributes".text = ''
        *.gpg filter=gpg diff=gpg
      '';
      environment.etc."nixos/${cfg.hooks.dirName}/pre-push/stop-wip" = {
        mode = "0644";
        user = config.attributes.mainUser.name;
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
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [
          epkgs.dired-git-info
          epkgs.git-msg-prefix
          epkgs.git-timemachine
          epkgs.magit
          epkgs.magit-filenotify
          epkgs.magit-popup # *
          epkgs.magit-todos
        ] ++ lib.optionals (cfg.strictRemoteBrowsing) [
          epkgs.browse-at-remote
        ] ++ lib.optionals (!cfg.strictRemoteBrowsing) [
          epkgs.git-link
        ];
      };
      ide.emacs.config = ''${emacsGitSetup}'';
    })
  ];
}

# * it seems some magit-dependent packages yet depend on magit-popup in some path, so we introduced
#   this explicit dependency and will keep it until transition to "transient" library is fully done
#   by all affected packages. (or some other root cause of "magit-popup"" will pop up)
