{ config, lib, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
with lib;

let
    cfg = config.services.git-save-wip;
    pathPkgs = with pkgs; [
        pass
        gitAndTools.pass-git-helper
    ];
in {
    options = {
        services.git-save-wip = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable fetching updates from upstream(s).
                '';
            };
            bootTimespec = mkOption {
                type = types.str;
                default = false;
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
                assertion = (cfg.bootTimespec == "" && cfg.activeTimespec == "" && cfg.calendarTimespec != "") ||
                            (cfg.bootTimespec != "" && cfg.activeTimespec != "" && cfg.calendarTimespec == "");
                message = "Must provide either calendarTimespec or bootTimespec/activeTimespec pair.";
            }
        ];

        # TODO: consider making programs.mr module
        systemd.user.services."git-save-wip" = {
            description = "Save work-in-progress in registered git repo(s)";
            path = pathPkgs;
            serviceConfig = {
                Type = "oneshot";
                ExecStart = "${pkgs.bash}/bin/bash -c \"[[ $(${pkgs.xprintidle-ng}/bin/xprintidle-ng) -ge $((3600*1000)) ]] && ${pkgs.mr}/bin/mr savewip\""; # TODO: only when not on master
                StandardOutput = "journal+console";
                StandardError = "inherit";
            };
        };
        systemd.user.timers."git-save-wip" = {
            description = "Save work-in-progress in registered git repo(s)";
            wantedBy = [ "timers.target" ];
            timerConfig = {
                OnBootSec = cfg.bootTimespec;
                OnUnitActiveSec = cfg.activeTimespec;
                OnCalendar = cfg.calendarTimespec;
            };
        };
    };
}
