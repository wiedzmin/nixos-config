{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.pim;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  nixpkgs-arbtt = import inputs.nixpkgs-arbtt ({
    config = config.nixpkgs.config;
    system = "x86_64-linux";
  });
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
      timeTracking.inactiveSec = mkOption {
        type = types.int;
        default = 60;
        description = "arbtt inactivity treshold.";
      };
      timeTracking.inactiveTag = mkOption {
        type = types.str;
        default = "inactive";
        description = "arbtt tag for idle time. `inactive` is treated specially.";
      };
      timeTracking.rules = mkOption {
        type = types.lines;
        default = "";
        description = "arbtt tagging rules.";
      };
      timeTracking.config = mkOption {
        type = types.lines;
        default = ''
          $idle > ${builtins.toString cfg.timeTracking.inactiveSec} ==> tag ${cfg.timeTracking.inactiveTag},
          -- tag apps:$current.program, -- just tags the current program (do we really need it?)

          current window ($program == "emacs" && $title =~ m!(?:/etc)/nixos/!) ==> tag project:nixos-config,

          ${cfg.timeTracking.rules}

          -- dates/time nitpicking
          year $date == 2014 ==> tag year:2014,
          year $date == 2015 ==> tag year:2015,
          year $date == 2016 ==> tag year:2016,
          year $date == 2017 ==> tag year:2017,
          year $date == 2018 ==> tag year:2018,
          year $date == 2019 ==> tag year:2019,
          year $date == 2020 ==> tag year:2020,

          month $date == 1 ==> tag month:January,
          month $date == 2 ==> tag month:February,
          month $date == 3 ==> tag month:March,
          month $date == 4 ==> tag month:April,
          month $date == 5 ==> tag month:May,
          month $date == 6 ==> tag month:June,
          month $date == 7 ==> tag month:July,
          month $date == 8 ==> tag month:August,
          month $date == 9 ==> tag month:September,
          month $date == 10 ==> tag month:October,
          month $date == 11 ==> tag month:November,
          month $date == 12 ==> tag month:December,

          day of week $date == 1 ==> tag week:Monday,
          day of week $date == 2 ==> tag week:Tuesday,
          day of week $date == 3 ==> tag week:Wednesday,
          day of week $date == 4 ==> tag week:Thursday,
          day of week $date == 5 ==> tag week:Friday,
          day of week $date == 6 ==> tag week:Saturday,
          day of week $date == 7 ==> tag week:Sunday,

          $sampleage <= 168:00 ==> tag current-week, -- last week
          $sampleage <= 24:00 ==> tag current-day, -- last 24h
          $sampleage <= 1:00 ==> tag last-hour, -- last hour

          $time >=  2:00 && $time <  8:00 ==> tag time-of-day:night,
          $time >=  8:00 && $time < 12:00 ==> tag time-of-day:morning,
          $time >= 12:00 && $time < 14:00 ==> tag time-of-day:lunchtime,
          $time >= 14:00 && $time < 18:00 ==> tag time-of-day:afternoon,
          $time >= 18:00 && $time < 22:00 ==> tag time-of-day:evening,
          $time >= 22:00 || $time <  2:00 ==> tag time-of-day:late-evening,

          -- !!! $now fails, probably because of ancient arbtt non-broken version !!!
          -- month $date == month $now ==> tag current-month,
          -- year $now == year $date ==> tag current-year,

          {-
          Some "nontrivial" examples (mostly copypaste):
          arbtt-stats -f 'month $date==3 && year $date==2018'
          arbtt-stats  --filter='$date>='`date +"%Y-%m-%d"` --each-category

          if current window ($desktop == "stuff") then current window ($program == "urxvt") ==> tag Program:mutt else tag Desktop:$desktop,

          condition
            isJava = current window $program == ["sun-awt-X11-XFramePeer", "sun-awt-X11-XDialogPeer", "sun-awt-X11-XWindowPeer"]
          in $isJava && current window $title == "I3P" ==> tag Program:I3P,

          format $date =~ ".*-03-19"  ==> tag period:on_a_special_day,
          format $date =~ /.*-03-19/  ==> tag period:on_a_special_day,

          current window $program == "Firefox" ==> {
            (current window $title =~ m!(?:subreddit of | r/)(ranger)! || current window $title =~ m!ranger/(ranger)!) ==> tag Project:$1,
            current window $title =~ m!· ([^/·]+) / ([^/·]+) · GitLab! ==> tag Project:$1__$2,
            current window $title =~ m!([^/ ]+)/([^/@: ]+)(?:$|@|:)! ==> tag Project:$1__$2,
          },
          -}
        '';
        visible = false;
        readOnly = true;
        internal = true;
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
      nixpkgs.config.packageOverrides = _: rec {
        tt_capture = mkPythonScriptWithDeps "tt_capture"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.cbor2 python3Packages.pytz python3Packages.xlib xprintidle-ng ])
          (readSubstituted ../subst.nix ./scripts/tt_capture.py);
      };
      services.arbtt = {
        enable = true;
        package = nixpkgs-arbtt.haskellPackages.arbtt;
      };
      home-manager.users."${user}" = {
        home.file = {
          ".arbtt/categorize.cfg".text = cfg.timeTracking.config;
        };
        home.packages = with pkgs;
          [
            nixpkgs-arbtt.haskellPackages.arbtt # for stats viewing
          ] ++ lib.optionals config.attributes.debug.scripts [ tt_capture ];
      };
    })
    (mkIf (cfg.enable && cfg.scheduling.enable) {
      home-manager.users."${user}" = {
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
      home-manager.users."${user}" = { home.packages = with pkgs; [ plantuml ]; };
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
