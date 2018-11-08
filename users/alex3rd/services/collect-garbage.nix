{config, pkgs, lib, ...}:

{
    systemd.services."collect-nix-garbage" = {
        description = "Collect garbage in running system";
        serviceConfig = {
            Type = "oneshot";
            User = "alex3rd"; # TODO: think of abstracting away
            ExecStart = "${pkgs.optimize-nix}/bin/optimize-nix";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."collect-nix-garbage" = {
        description = "Collect garbage in running system";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1day";
            OnUnitActiveSec = "1week";
        };
    };
}
