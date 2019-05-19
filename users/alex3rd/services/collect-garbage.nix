{config, pkgs, lib, ...}:

# TODO: think of moving to system level
{
    systemd.services."collect-nix-garbage" = {
        description = "Collect garbage in running system";
        serviceConfig = {
            Type = "oneshot";
            User = "root";
            ExecStart = "${pkgs.optimize-nix}/bin/optimize-nix";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."collect-nix-garbage" = {
        description = "Collect garbage in running system";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1hour";
            OnUnitActiveSec = "1day";
        };
    };
}
