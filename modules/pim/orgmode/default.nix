{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.pim.orgmode;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    pim.orgmode = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Org mode setup";
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
      agendaElPatch = mkOption {
        type = types.lines;
        # FIXME: check not only for Firefox sessions path (below)
        default = ''
          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (root: delay: ''
            (deferred:nextc
              (deferred:wait-idle ${builtins.toString delay})
              (lambda () (f-entries "${root}"
                                    (lambda (entry) (when (and (f-file? entry)
                                                               (s-suffix? ".org" entry)
                                                               (not (s-prefix? "${config.browsers.firefox.sessions.path}" entry))
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
        org-capture = mkPythonScriptWithDeps "org-capture" (with pkgs; [ emacs nurpkgs.pystdlib tmux xsel ])
          (readSubstituted ../../subst.nix ./scripts/org-capture.py);
      };

      custom.programs.tmux.bindings.copyMode = { "M-n" = ''run-shell "${pkgs.org-capture}/bin/org-capture ns"''; };
      pim.orgmode.agendaRoots = { "${config.ide.emacs.core.orgDir}" = 3000; };
      pim.timetracking.rules = ''
        current window ($title =~ /^emacs - [^ ]+\.org .*$/) ==> tag edit:orgmode,
      '';
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.deft
        epkgs.doct
        epkgs.helm-org-rifle
        epkgs.ob-async
        epkgs.ob-blockdiag
        epkgs.ob-restclient
        epkgs.org-bullets
        epkgs.org-capture-pop-frame
        epkgs.org-clock-today
        epkgs.org-drill
        epkgs.org-plus-contrib
        epkgs.org-pomodoro
        epkgs.org-ql
        epkgs.org-randomnote
        epkgs.org-recent-headings
        epkgs.org-rich-yank
        epkgs.org-sticky-header
        epkgs.orgit
        epkgs.russian-holidays
      ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/orgmode.el;
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ org-capture ]; };
    })
  ];
}
