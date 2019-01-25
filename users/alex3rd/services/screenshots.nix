{config, pkgs, lib, ...}:

{
    systemd.services."order-screenshots" = {
        description = "Timely order screenshots";
        serviceConfig = {
            Type = "oneshot";
            User = "alex3rd"; # TODO: think of abstracting away
            ExecStart = "${pkgs.order_screenshots}/bin/order_screenshots";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."order-screenshots" = {
        description = "Timely order screenshots";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "5min";
            OnUnitActiveSec = "1d";
        };
    };
}
