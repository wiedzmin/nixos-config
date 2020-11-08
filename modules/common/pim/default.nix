{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.pim;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    custom.pim = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable personal information management infra/tools.";
      };
      timeTracking.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable personal time-tracking infra/tools.";
      };
      org.warningsFile = mkOption {
        type = types.str;
        default = "$HOME/warnings.org";
        description = "Org-mode file to place accidental deletes diff.";
      };
      org.agendaUpdateDelay = mkOption {
        type = types.int;
        default = 15000;
        description = "Msec amount of Emacs idle time to bass before updating Org agenda.";
      };
      org.agendaRoots = mkOption {
        type = types.attrs;
        default = { };
        description = ''
          Paths to search Org files for agenda.

          Each entry associates with msec timedelta, which means
          the amount of idle Emacs time to pass before performing
          particular path crawling.
        '';
      };
      org.agendaElPatch = mkOption {
        type = types.lines;
        default = ''
          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (root: delay: ''
            (deferred:nextc
              (deferred:wait-idle ${builtins.toString delay})
              (lambda () (f-entries "${root}"
                                    (lambda (entry) (when (and (f-file? entry)
                                                               (s-suffix? ".org" entry)
                                                               (not (s-prefix? "${config.custom.browsers.firefox.sessions.path}" entry))
                                                               (not (s-contains? "journal" entry)) ;; maybe make option for such ignores
                                                               (file-exists-p entry))
                                                      (push entry org-agenda-files))) t)))
          '') cfg.org.agendaRoots)}
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = "Elisp code to insert to orgmode configuration.";
      };
      scheduling.enable = mkOption {
        type = types.bool;
        description = ''
          Whether to enable scheduled tasks, such as opening browser with links,
          starting applications or so.
        '';
        default = false;
      };
      scheduling.entries = mkOption {
        type = types.attrs;
        example = {
          "read_mail" = {
            cal = "Mon,Tue *-*-01..04 12:00:00";
            cmd = "${config.attributes.browser.fallback} https://mail.google.com";
          };
        };
        default = { };
        description = ''
          Scheduled task entries.

          Timestamp for task issuing should be presented in systemd timers' OnCalendar entries format.
          Task definition is simple a shell command line to execute.
        '';
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs pim-related setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.timeTracking.enable) {
      assertions = [{
        assertion = cfg.timeTracking.enable && builtins.pathExists (homePrefix ".arbtt/categorize.cfg");
        message = "pim: no arbtt configuration found.";
      }];

      nixpkgs.config.packageOverrides = _: rec {
        tt_capture = mkPythonScriptWithDeps "tt_capture"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.cbor2 python3Packages.pytz python3Packages.xlib xprintidle-ng ])
          (readSubstituted ../subst.nix ./scripts/tt_capture.py);
      };
      services.arbtt = {
        enable = true;
        package = inputs.nixpkgs-16_04_20.legacyPackages.x86_64-linux.haskellPackages.arbtt;
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs;
          [
            inputs.nixpkgs-16_04_20.haskellPackages.arbtt # for stats viewing
          ] ++ lib.optionals config.attributes.debug.scripts [ tt_capture ];
      };
    })
    (mkIf (cfg.enable && cfg.scheduling.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.activation.ensureSchedulingTimers = {
          after = [ ];
          before = [ "checkLinkTargets" ];
          # FIXME: parameterize DBUS_SESSION_BUS_ADDRESS value
          data = ''
            export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
            ${lib.concatStringsSep "\n"
              (lib.mapAttrsToList (name: _: "${pkgs.systemd}/bin/systemctl --user restart ${name}.timer")
                cfg.scheduling.entries)}
          '';
        };
      };
      systemd.user.services = lib.mapAttrs (name: meta: {
        description = "${name}";
        serviceConfig = {
          Type = "oneshot";
          Environment = [ "DISPLAY=:0" ];
          ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
          ExecStart = "${meta.cmd}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      }) cfg.scheduling.entries;
      systemd.user.timers = lib.mapAttrs (name: meta: {
        description = "${name}";
        wantedBy = [ "timers.target" ];
        timerConfig = { OnCalendar = meta.cal; };
      }) cfg.scheduling.entries;
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      custom.pim.org.agendaRoots = { "${config.ide.emacs.orgDir}" = 3000; };
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ plantuml ]; };
      ide.emacs.extraPackages = epkgs: [
        epkgs.counsel-org-clock
        epkgs.deft
        epkgs.doct
        epkgs.helm-org-rifle
        epkgs.ivy-omni-org
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
        epkgs.plantuml-mode
        epkgs.russian-holidays
      ];
      ide.emacs.config = readSubstituted ../subst.nix ./emacs/pim.el;
    })
  ];
}
