{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.pim.orgmode;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        org-capture = mkPythonScriptWithDeps pkgs "org-capture" (with pkgs; [ emacs nurpkgs.pystdlib tmux xsel ])
          (builtins.readFile ./scripts/org-capture.py);
      };

      ext.programs.tmux.bindings.copyMode = { "M-n" = ''run-shell "${pkgs.org-capture}/bin/org-capture ns"''; };
      pim.orgmode.agendaRoots = { "${cfg.rootDir}" = 3000; };
      pim.timetracking.rules = mkArbttTitleRule [ "^emacs - [^ ]+\\.org .*$" ] "edit:orgmode";
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.deft
        epkgs.doct
        epkgs.helm-org-rifle
        epkgs.ob-async
        epkgs.ob-blockdiag
        epkgs.ob-restclient
        epkgs.org-appear
        epkgs.org-bullets
        epkgs.org-clock-today
        epkgs.org-contrib
        epkgs.org-pomodoro
        epkgs.org-ql
        epkgs.org-randomnote
        epkgs.org-recent-headings
        epkgs.org-rich-yank
        epkgs.org-roam
        epkgs.orgit
        epkgs.russian-holidays
      ] ++ optionals cfg.cliplink.enable [ epkgs.org-cliplink ];
      ide.emacs.core.config = (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./emacs/orgmode.el ])
        + lib.optionalString cfg.cliplink.enable ''
        (use-package org-cliplink
          :after (org)
          :bind
          (:map custom-org-map
                ("i" . org-cliplink)))
      '';
      ide.emacs.core.customKeymaps = {
        "custom-org-map" = "<f7>";
        "org-roam-map" = "<f7> r";
      };
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ org-capture ]; };
    })
  ];
}
