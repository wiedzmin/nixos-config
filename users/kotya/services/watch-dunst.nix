{config, pkgs, lib, ...}:
with import ../const.nix {inherit config pkgs;};
{
    imports = [
        ../../../toolbox/scripts/misc.nix
    ];

    systemd.services."watch-dunst" = {
        description = "Keep dunst running properly";
        path = [ pkgs.logger ];
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
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
