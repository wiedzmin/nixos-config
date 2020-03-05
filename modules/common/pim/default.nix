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
            (builtins.attrValues (lib.filterAttrs (n: v: n != "global") config.custom.dev.workspaceRoots))
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
    (mkIf (cfg.enable) {
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
        home.file = {
          ".arbtt/categorize.cfg".text = ''
            aliases (
                    "Navigator" -> "Firefox",
                    "Zathura" -> "PDF reader",
                    "telegram-desktop" -> "Telegram",
                    "Alacritty" -> "Shell",
            )

            $idle > 60 ==> tag inactive,
            current window $program == ["Navigator", "Google-chrome", "Google-chrome-stable"] ==> tag activity:web,
            current window $program == ["Zathura"] ==> tag activity:pdf,
            current window $program == ["FBReader"] ==> tag activity:fiction,

            tag apps:$current.program, -- just tags the current program

            -- projects at work
            current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
              ==> tag project:$1-$2,
            current window ($program == "Alacritty" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
              ==> tag project:$1-$2,

            -- personal projects
            current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/workspace/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/!)
              ==> tag project:$1-$2,
            current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/.xmonad/!) ==> tag project:xmonad-config,
            current window ($program == "Alacritty" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/.xmonad/!) ==> tag project:xmonad-config,
            current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/.emacs.d/!) ==> tag project:emacs-config,
            current window ($program == "emacs" && $title =~ m!(?:/etc)/nixos/!) ==> tag project:nixos-config,

            current window ($program == "Navigator" && $title =~ /Facebook/) ==> tag site:facebook,
            current window ($program == "Navigator" && $title =~ /Gmail/) ==> tag web:Gmail,
            current window ($program == "Navigator" && $title =~ /Google/) ==> tag web:Google,
            current window ($program == "Navigator" && $title =~ /wikipedia/) ==> tag site:wikipedia,
            current window ($program == "Navigator" && $title =~ /habr/) ==> tag site:habr,
            current window ($program == "Navigator" && $title =~ /pypi/) ==> tag site:pypi,
            current window ($program == "Navigator" && $title =~ /stackoverflow/) ==> tag site:stackoverflow,

            current window ($title =~ /^emacs - [^ ]+\.c .*$/) ==> tag edit:c,
            current window ($title =~ /^emacs - [^ ]+\.py .*$/) ==> tag edit:python,
            current window ($title =~ /^emacs - [^ ]+\.hs .*$/) ==> tag edit:haskell,
            current window ($title =~ /^emacs - [^ ]+\.lisp .*$/) ==> tag edit:cl,
            current window ($title =~ /^emacs - [^ ]+\.el .*$/) ==> tag edit:elisp,
            current window ($title =~ /^emacs - [^ ]+config\.org .*$/) ==> tag edit:elisp,
            current window ($title =~ /^emacs - [^ ]+\.pdf .*$/) ==> tag activity:pdf,

            -- $time evaluates to local time.
            $time >=  2:00 && $time <  8:00 ==> tag time-of-day:night,
            $time >=  8:00 && $time < 12:00 ==> tag time-of-day:morning,
            $time >= 12:00 && $time < 14:00 ==> tag time-of-day:lunchtime,
            $time >= 14:00 && $time < 18:00 ==> tag time-of-day:afternoon,
            $time >= 18:00 && $time < 22:00 ==> tag time-of-day:evening,
            $time >= 22:00 || $time <  2:00 ==> tag time-of-day:late-evening,

            -- This tag always refers to the last 24h
            $sampleage <= 24:00 ==> tag last-day,
            -- ...and last hour respectively
            $sampleage <= 1:00 ==> tag last-hour,

            -- year/months
            year $date == 2014 ==> tag year:2014,
            year $date == 2015 ==> tag year:2015,
            year $date == 2016 ==> tag year:2016,
            year $date == 2017 ==> tag year:2017,
            year $date == 2018 ==> tag year:2018,
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

            day of month $now == day of month $date ==> tag current-day,
            day of week $date == 1 ==> tag week:Monday,
            day of week $date == 2 ==> tag week:Tuesday,
            day of week $date == 3 ==> tag week:Wednesday,
            day of week $date == 4 ==> tag week:Thursday,
            day of week $date == 5 ==> tag week:Friday,
            day of week $date == 6 ==> tag week:Saturday,
            day of week $date == 7 ==> tag week:Sunday,

            month $now == month $date ==> tag current-month,
            year $now == year $date ==> tag current-year,
          '';
        };
        home.packages = with pkgs; [
          nixpkgs-pinned-05_12_19.haskellPackages.arbtt # for stats viewing

          remind # + rem2ics (make overlay)
          wyrd

          tt_capture
        ];
      };
    })
    (mkIf (cfg.enable && cfg.scheduling.enable) {
      systemd.user.services = lib.mapAttrs (name: meta: {
        description = "${name}";
        serviceConfig = {
          Type = "oneshot";
          ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
          ExecStart = "${meta.cmd}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
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
