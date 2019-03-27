{config, pkgs, lib, ...}:
with import ../const.nix {inherit config pkgs;};
{
    systemd.services."nixpkgs-update-status" = {
        description = "Timely check for nixpkgs updates";
        path = [ pkgs.logger ];
        environment = {
            NEEDFETCH_MESSAGE = "Nixpkgs updates available!";
        };
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
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
