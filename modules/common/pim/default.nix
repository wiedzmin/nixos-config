let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-05_12_19 = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.custom.pim;
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
        default = {
          "${config.ide.emacs.orgDir}" = 3000;
        } // lib.optionalAttrs (config.custom.dev.workspaceRoots != { })
          (lib.genAttrs
            (builtins.attrValues (lib.filterAttrs (n: _: n != "global") config.custom.dev.workspaceRoots))
            (_: cfg.org.agendaUpdateDelay));
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
          ${lib.concatStringsSep "\n"
            (lib.mapAttrsToList (root: delay: ''
              (deferred:nextc
                (deferred:wait-idle ${builtins.toString delay})
                (lambda () (f-entries "${root}"
                                      (lambda (entry) (when (and (f-file? entry)
                                                                 (s-suffix? ".org" entry)
                                                                 (not (s-prefix? "${config.custom.browsers.sessions.firefox.path}" entry))
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
            cmd = "${config.attributes.defaultCommands.browser} https://mail.google.com";
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
      assertions = [{ # FIXME: if this would not work, then try checking if corresponding secrets are enabled
        assertion = cfg.timeTracking.enable && builtins.pathExists "/home/${config.attributes.mainUser.name}/.arbtt/categorize.cfg";
        message = "pim: no arbtt configuration found.";
      }];

      nixpkgs.config.packageOverrides = _: rec {
        tt_capture = writePythonScriptWithPythonPackages "tt_capture" [
          pkgs.python3Packages.cbor2
          pkgs.python3Packages.pytz
          pkgs.python3Packages.xlib
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./tt_capture.py; })));
      };
      services.arbtt = {
        enable = true;
        package = nixpkgs-pinned-05_12_19.haskellPackages.arbtt;
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nixpkgs-pinned-05_12_19.haskellPackages.arbtt # for stats viewing
          tt_capture
        ];
      };
    })
    (mkIf (cfg.enable && cfg.scheduling.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          remind # + rem2ics (make overlay)
          wyrd
        ];
      };
      systemd.user.services = lib.mapAttrs (name: meta: {
        description = "${name}";
        serviceConfig = {
          Type = "oneshot";
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
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ plantuml ];
        programs.emacs.extraPackages = epkgs: [
          epkgs.counsel-org-clock
          epkgs.deft
          epkgs.ivy-omni-org
          epkgs.ob-async
          epkgs.ob-blockdiag
          epkgs.ob-restclient
          epkgs.org-bullets
          epkgs.org-capture-pop-frame
          epkgs.org-clock-today
          epkgs.org-plus-contrib
          epkgs.org-pomodoro
          epkgs.org-randomnote
          epkgs.org-recent-headings
          epkgs.org-rich-yank
          epkgs.org-sticky-header
          epkgs.orgit
          epkgs.orglink
          epkgs.plantuml-mode
          epkgs.russian-holidays
        ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./pim.el; }));
    })
  ];
}
