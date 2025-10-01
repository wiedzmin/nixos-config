{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.pim.orgmode;
  user = config.attributes.mainUser.name;
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
      pim.orgmode.agendaRoots = { "${cfg.rootDir}" = 3000; };
      pim.timetracking.rules = mkArbttTitleRule [ "^emacs - [^ ]+\\.org .*$" ] "edit:orgmode";
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.doct
        epkgs.ob-async
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
        epkgs.russian-holidays
      ] ++ optionals cfg.cliplink.enable [ epkgs.org-cliplink ]
      ++ optionals (config.ide.emacs.navigation.collections.backend == "consult") [ epkgs.consult-org-roam ]
      ++ optionals (!config.pim.core.enable) [
        epkgs.orgit
        epkgs.orglink
      ];
      ide.emacs.core.config = (readSubstituted config inputs pkgs [ ./subst/orgmode.nix ] [ ./elisp/orgmode.el ])
        + (optionalString (config.ide.emacs.navigation.collections.backend == "consult") readSubstituted config inputs pkgs [ ./subst/consult.nix ] [ ./elisp/consult.el ])
        + (optionalString (!config.pim.core.enable) (builtins.readFile ./elisp/org-ext.el))
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
      ide.emacs.completion.tempel.snippets = ''
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
      ide.emacs.core.treesitter.grammars = {
        org = "https://github.com/milisims/tree-sitter-org";
      };
    })
    (mkIf (cfg.enable && cfg.org-roam.enable) {
      home-manager.users."${user}" = {
        programs.qutebrowser = {
          keyBindings = {
            normal = {
              # FIXME: does not work yet
              "<Ctrl-Shift-r>" = "open javascript:location.href='org-protocol://roam-ref?template=d&ref=+encodeURIComponent'(location.href)+'&title='+encodeURIComponent(document.title)";
              "<Ctrl-Shift-к>" = "open javascript:location.href='org-protocol://roam-ref?template=d&ref='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)";
            };
          };
        };
      };

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.org-roam
        epkgs.org-roam-ui
      ];
      ide.emacs.core.config = (readSubstituted config inputs pkgs [ ./subst/org-roam.nix ] [ ./elisp/org-roam.el ]);
      ide.emacs.core.customKeymaps = {
        "org-roam-map" = "<f7> r";
      };
    })
    (mkIf (cfg.enable && cfg.blings.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.org-bullets
      ];
      ide.emacs.core.config = (readSubstituted config inputs pkgs [ ./subst/blings.nix ] [ ./elisp/blings.el ]);
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "discourse/org-roam" = {
          desc = "Org-roam Discourse";
          remote = {
            url = "https://org-roam.discourse.group/";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
        "orgupd" = {
          desc = "Orgmode updates";
          remote = {
            url = "https://updates.orgmode.org/";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
        "orui" = {
          desc = "Org-roam UI";
          remote = {
            url = "http://127.0.0.1:35901/";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
        "jethrokuan/braindump/src" = {
          desc = "Jethrokuan's braindump src";
          remote = {
            url = "https://github.com/jethrokuan/braindump/tree/master/org";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
        "jethrokuan/braindump" = {
          desc = "Jethrokuan's braindump";
          remote = {
            url = "https://braindump.jethro.dev/";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
        "org-roam/proto" = {
          desc = "org-protocol docs for org-roam";
          remote = {
            url = "https://www.orgroam.com/manual.html#The-roam_002dnode-protocol";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
        "worg" = {
          desc = "Hello Worg, the Org-Mode Community!";
          remote = {
            url = "https://orgmode.org/worg/index.html";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.org-roam.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        emacs_orgmode = {
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
