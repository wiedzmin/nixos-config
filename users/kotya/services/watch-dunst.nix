{config, pkgs, lib, ...}:

{
    imports = [
        ../../../toolbox/scripts/misc.nix
    ];

    systemd.services."watch-dunst" = {
        description = "Keep dunst running properly";
        path = [ pkgs.logger ];
        serviceConfig = {
            Type = "oneshot";
            User = "alex3rd"; # TODO: think of abstracting away
            ExecStart = "${pkgs.watch_dunst}/bin/watch_dunst";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."watch-dunst" = {
        description = "Keep dunst running properly";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1min";
            OnUnitActiveSec = "5min";
        };
    };
}
