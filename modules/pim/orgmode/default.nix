{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.pim.orgmode;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    pim.orgmode = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Org mode setup";
      };
      rootDir = mkOption {
        type = types.str;
        default = homePrefix user "docs/org";
        description = ''
          Path to store org-mode docs under.
        '';
      };
      cliplink.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use `org-cliplink` for capturing";
      };
      warningsFile = mkOption {
        type = types.str;
        default = "$HOME/warnings.org";
        description = "Org-mode file to place accidental deletes diff";
      };
      agendaUpdateDelay = mkOption {
        type = types.int;
        default = 15000;
        description = "Msec amount of Emacs idle time to pass before updating Org agenda";
      };
      agendaRoots = mkOption {
        type = types.attrs;
        default = { };
        description = ''
          Paths to search Org files for agenda.

          Each entry associates with msec timedelta, which means
          the amount of idle Emacs time to pass before performing
          particular path crawling.
        '';
      };
      commonCaptureDataTemplate = mkOption {
        type = types.str;
        # default = (if cfg.cliplink.enable then "%(org-cliplink-capture)" else "%?[[%:link][%:description]]");
        default = if cfg.cliplink.enable then "%(org-cliplink-capture)" else "%?";
        description = "Default data template for captured entry";
        visible = false;
        readOnly = true;
        internal = true;
      };
      agendaElPatch = mkOption {
        type = types.lines;
        default = ''
          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (root: delay: ''
            (deferred:nextc
              (deferred:wait-idle ${builtins.toString delay})
              (lambda () (f-entries "${root}"
                                    (lambda (entry) (when (and (f-file? entry)
                                                               (s-suffix? ".org" entry)
                                                               (not (s-prefix? "${config.browsers.firefox.sessions.path}" entry))
                                                               (not (s-prefix? "${config.browsers.qutebrowser.sessions.path}" entry))
                                                               (not (s-contains? "journal" entry)) ;; maybe make option for such ignores
                                                               (file-exists-p entry))
                                                      (push entry org-agenda-files))) t)))
          '') cfg.agendaRoots)}
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = "Elisp code to insert to orgmode configuration.";
      };
      org-roam.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable org-roam functionality";
      };
      org-roam.rootDir = mkOption {
        type = types.str;
        default = "${cfg.rootDir}/roam";
        description = ''
          Path to store org-roam docs under.
        '';
      };
      org-roam.autosync.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable org-roam DB auto-synchronization";
      };
      blings.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Orgmode blinging";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Orgmode-related bookmarks";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: {
        org-capture = mkPythonScriptWithDeps pkgs "org-capture" (with pkgs; [ emacs nurpkgs.pystdlib tmux xsel ])
          (builtins.readFile ./scripts/org-capture.py);
      };

      ext.programs.tmux.bindings.copyMode = { "M-n" = ''run-shell "${pkgs.org-capture}/bin/org-capture ns"''; };
      pim.orgmode.agendaRoots = { "${cfg.rootDir}" = 3000; };
      pim.timetracking.rules = mkArbttTitleRule [ "^emacs - [^ ]+\\.org .*$" ] "edit:orgmode";
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.doct
        epkgs.ob-async
        epkgs.ob-blockdiag
        epkgs.ob-restclient
        epkgs.org-appear
        epkgs.org-clock-today
        epkgs.org-contrib
        epkgs.org-edit-indirect
        epkgs.org-pomodoro
        epkgs.org-ql
        epkgs.org-randomnote
        epkgs.org-recent-headings
        epkgs.org-rich-yank
        epkgs.orgit
        epkgs.orglink
        epkgs.russian-holidays
      ] ++ optionals cfg.cliplink.enable [ epkgs.org-cliplink ]
      ++ optionals (config.ide.emacs.navigation.collections.backend == "consult") [ epkgs.consult-org-roam ];
      ide.emacs.core.config = (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/orgmode.el ])
        + (optionalString (config.ide.emacs.navigation.collections.backend == "consult") readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/consult.el ])
        + lib.optionalString cfg.cliplink.enable ''
        (use-package org-cliplink
          :after (org)
          :bind
          (:map custom-org-map
                ("i" . org-cliplink)))
      '';
      ide.emacs.core.customKeymaps = {
        "custom-org-map" = "<f7>";
      };
      completion.emacs.tempel.snippets = ''
        org-mode

        (caption "#+caption: ")
        (title "#+title: ")
        (author "#+author: ")
        (drawer ":" p ":" n r ":end:")
        (begin "#+begin_" (s name) n> r> n "#+end_" name)
        (quote "#+begin_quote" n> r> n "#+end_quote")
        (example "#+begin_example" n> r n "#+end_example")
        (src "#+begin_src " q n> r> n "#+end_src")
      '';
    })
    (mkIf (cfg.enable && cfg.org-roam.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.org-roam
        epkgs.org-roam-ui
      ];
      ide.emacs.core.config = (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/org-roam.el ]);
      ide.emacs.core.customKeymaps = {
        "org-roam-map" = "<f7> r";
      };
    })
    (mkIf (cfg.enable && cfg.blings.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.org-bullets
      ];
      ide.emacs.core.config = (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/blings.el ]);
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ org-capture ]; };
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "discourse/org-roam" = {
          desc = "Org-roam Discourse";
          remote.url = "https://org-roam.discourse.group/";
        };
        "orgupd" = {
          desc = "Orgmode updates";
          remote.url = "https://updates.orgmode.org/";
        };
        "orui" = {
          desc = "Org-roam UI";
          remote.url = "http://127.0.0.1:35901/";
        };
        "jethrokuan/braindump/src" = {
          desc = "Jethrokuan's braindump src";
          remote.url = "https://github.com/jethrokuan/braindump/tree/master/org";
        };
        "jethrokuan/braindump" = {
          desc = "Jethrokuan's braindump";
          remote.url = "https://braindump.jethro.dev/";
        };
      };
    })
    (mkIf (cfg.enable && cfg.org-roam.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/match/emacs_orgmode.yml".source = yaml.generate "espanso-emacs_orgmode.yml" {
          matches = [
            {
              trigger = ":orc";
              replace = "cd ${cfg.org-roam.rootDir} && find . -name \"*.org\" | xargs wc -l | sort -k2 | ${pkgs.moar}/bin/moar";
            }
            {
              trigger = ":sorc";
              replace = "cd ${cfg.org-roam.rootDir} && find . -name \"*.org\" | xargs wc -l | sort -k1n | ${pkgs.moar}/bin/moar";
            }
            {
              trigger = ":sforc";
              replace = "cd ${cfg.org-roam.rootDir} && find . -name \"*.org\" | xargs wc -l | sort -k1n | ${pkgs.fzf}/bin/fzf --tac";
            }
            {
              trigger = ":sForc";
              replace = "cd ${cfg.org-roam.rootDir} && find . -name \"*.org\" | xargs wc -l | sort -k1n | ${pkgs.fzf}/bin/fzf";
            }
          ];
        };
      };
    })
  ];
}
