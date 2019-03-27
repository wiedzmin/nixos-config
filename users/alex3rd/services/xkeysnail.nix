{config, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
{
    systemd.services."xkeysnail" = {
        enable = true;
        description = "Xkeysnail";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        environment = {
            DISPLAY = ":0";
            XAUTHORITY = "/home/${userName}/.Xauthority";
        };
        serviceConfig = {
            PIDFile = "/var/run/xkeysnail.pid";
            Restart = "always";
            RestartSec = 1;
            ExecStart = "${pkgs.xkeysnail}/bin/xkeysnail /home/${userName}/.config/xkeysnail/config.py";
        };
    };
}
