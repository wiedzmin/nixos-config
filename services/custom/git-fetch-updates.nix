{config, pkgs, lib, ...}:

{
    systemd.services."git-fetch-updates-work" = {
        description = "Fetch updates from Git repos";
        path = [ pkgs.pass pkgs.gitAndTools.pass-git-helper ];
        serviceConfig = {
            Type = "oneshot";
            User = "alex3rd"; # TODO: think of abstracting away
            ExecStart = "${pkgs.git-fetch-batch}/bin/git-fetch-batch ${config.job_workspace_path}";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."git-fetch-updates-work" = {
        description = "Fetch updates from Git repos";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1min";
            OnUnitActiveSec = "1hour";
        };
    };
}
