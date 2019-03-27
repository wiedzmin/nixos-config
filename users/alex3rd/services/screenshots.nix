{config, pkgs, lib, ...}:
with import ../const.nix {inherit config pkgs;};
{
    systemd.services."order-screenshots" = {
        description = "Timely order screenshots";
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
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
