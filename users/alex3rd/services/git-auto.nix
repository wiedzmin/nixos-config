{config, pkgs, lib, ...}:

{
    imports = [
        ../private/traits.nix
        ../scripts/git.nix
    ];
    systemd.services."git-fetch-updates-work" = {
        description = "Fetch updates from work git repos";
        path = [ pkgs.pass pkgs.gitAndTools.pass-git-helper ];
        serviceConfig = {
            Type = "oneshot";
            User = "alex3rd"; # TODO: think of abstracting away
            ExecStart = "${pkgs.git-fetch-batch}/bin/git-fetch-batch ${config.job.workspace_path}";
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
    systemd.services."git-save-wip-work" = {
        description = "Commit WIP changes to work repos";
        path = [ pkgs.pass pkgs.gitAndTools.pass-git-helper pkgs.gitAndTools.stgit ];
        serviceConfig = {
            Type = "oneshot";
            User = "alex3rd"; # TODO: think of abstracting away
            ExecStart = "${pkgs.git-save-wip-batch}/bin/git-save-wip-batch ${config.job.workspace_path}";
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
    systemd.services."git-save-wip-pets" = {
        description = "Commit WIP changes to pet projects' repos";
        path = [ pkgs.pass pkgs.gitAndTools.pass-git-helper pkgs.gitAndTools.stgit ];
        serviceConfig = {
            Type = "oneshot";
            User = "alex3rd"; # TODO: think of abstracting away
            ExecStart = "${pkgs.git-save-wip-batch}/bin/git-save-wip-batch" + (lib.concatMapStrings (loc: " ${loc}") config.dev.pet_project_locations);
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."git-save-wip-pets" = {
        description = "Commit WIP changes to pet projects' repos";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1min";
            OnUnitActiveSec = "30min";
        };
    };
}
