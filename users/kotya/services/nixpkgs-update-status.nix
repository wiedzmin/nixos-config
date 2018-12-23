{config, pkgs, lib, ...}:

{
    systemd.services."nixpkgs-update-status" = {
        description = "Timely check for nixpkgs updates";
        path = [ pkgs.logger ];
        environment = {
            NEEDFETCH_MESSAGE = "Nixpkgs updates available!";
        };
        serviceConfig = {
            Type = "oneshot";
            User = "alex3rd"; # TODO: think of abstracting away
            ExecStart = "${pkgs.watch_nixpkgs_updates}/bin/watch_nixpkgs_updates";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."nixpkgs-update-status" = {
        description = "Timely check for nixpkgs updates";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "5min";
            OnUnitActiveSec = "3hour";
        };
    };
}
