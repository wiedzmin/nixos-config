{ config, lib, pkgs, ...}:
with lib;

let
    cfg = config.services.git-fetch-updates;
    pathPkgs = with pkgs; [
        pass
        gitAndTools.pass-git-helper
    ];
in {
    options = {
        services.git-fetch-updates = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable fetching updates from upstream(s).
                '';
            };
            workDir = mkOption {
                type = types.str;
                default = "";
                description = ''
                    Path to check for Myrepos configuration(s).
                '';
            };
            bootTimespec = mkOption {
                type = types.str;
                default = "";
                description = ''
                    Interval to activate service after system boot (in systemd format).
                '';
            };
            activeTimespec = mkOption {
                type = types.str;
                default = "";
                description = ''
                    Interval to activate service while system runs (in systemd format).
                '';
            };
            calendarTimespec = mkOption {
                type = types.str;
                default = "";
                description = ''
                    Timestamp of service activation (in systemd format).
                '';
            };
        };
    };

    config = mkIf cfg.enable {
        assertions = [
            {
                assertion = cfg.workDir != "";
                message = "git-fetch-updates: Must provide path to working directory.";
            }
            {
                assertion = (cfg.bootTimespec == "" && cfg.activeTimespec == "" && cfg.calendarTimespec != "") ||
                            (cfg.bootTimespec != "" && cfg.activeTimespec != "" && cfg.calendarTimespec == "");
                message = "git-fetch-updates: Must provide either calendarTimespec or bootTimespec/activeTimespec pair.";
            }
        ];

        systemd.user.services."git-fetch-updates" = {
            description = "Fetch updates from registered git upstream(s)";
            path = pathPkgs;
            serviceConfig = {
                Type = "oneshot";
                ExecStart = "${pkgs.mr}/bin/mr update";
                WorkingDirectory = cfg.workDir;
                StandardOutput = "journal+console";
                StandardError = "inherit";
            };
        };
        systemd.user.timers."git-fetch-updates" = {
            description = "Fetch updates from registered git upstream(s)";
            wantedBy = [ "timers.target" ];
            timerConfig = if (cfg.bootTimespec != "") then {
                OnBootSec = cfg.bootTimespec;
                OnUnitActiveSec = cfg.activeTimespec;
            } else {
                OnCalendar = cfg.calendarTimespec;
            };
        };
    };
}
