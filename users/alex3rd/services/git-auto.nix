{config, pkgs, lib, ...}:
with import ../const.nix {inherit config pkgs;};
{
    systemd.services."git-fetch-updates-work" = {
        description = "Fetch updates from work git repos";
        path = with pkgs; [ pass gitAndTools.pass-git-helper mr ];
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
            ExecStart = "mr update";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."git-fetch-updates-work" = {
        description = "Fetch updates from work git repos";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1min";
            OnUnitActiveSec = "1hour";
        };
    };
    systemd.services."git-push-updates-work" = {
        description = "Push updates to work git repos";
        path = with pkgs; [ pass gitAndTools.pass-git-helper mr ];
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
            ExecStart = "mr push";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."git-push-updates-work" = {
        description = "Push updates to work git repos";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnCalendar = "*-*-* 18:00:00";
        };
    };
    systemd.services."git-save-wip-work" = {
        description = "Commit WIP changes to work repos";
        path = with pkgs; [ pass gitAndTools.pass-git-helper ];
        environment = {
            DISPLAY = ":0";
            XAUTHORITY = "/home/${userName}/.Xauthority";
        };
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
            ExecStart = "${pkgs.bash}/bin/bash -c \"[[ $(${pkgs.xprintidle-ng}/bin/xprintidle-ng) -ge $((3600*1000)) ]] && ${pkgs.mr}/bin/mr savewip\"";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."git-save-wip-work" = {
        description = "Commit WIP changes to work repos";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1min";
            OnUnitActiveSec = "30min";
        };
    };
}
