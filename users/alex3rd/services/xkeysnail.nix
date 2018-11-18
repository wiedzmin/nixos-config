{config, pkgs, ...}:

{
    systemd.services."xkeysnail" = {
        enable = true;
        description = "Xkeysnail";
        wantedBy = [ "graphical-session.target" ];
        path = [ pkgs.xkeysnail ];
        environment = {
            DISPLAY = ":0";
            XAUTHORITY = "${config.users.extraUsers.alex3rd.home}/.Xauthority";
        };
        serviceConfig = {
            PIDFile = "/var/run/xkeysnail.pid";
            Restart = "always";
            RestartSec = 1;
            User="root";
            ExecStart = "${pkgs.xkeysnail}/bin/xkeysnail ${config.users.extraUsers.alex3rd.home}/.config/xkeysnail/config.py";
        };
    };
}
