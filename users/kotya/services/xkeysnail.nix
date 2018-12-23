{config, pkgs, ...}:

{
    systemd.services."xkeysnail" = {
        enable = true;
        description = "Xkeysnail";
        wantedBy = [ "graphical.target" ];
        environment = {
            DISPLAY = ":0";
            XAUTHORITY = "${config.users.extraUsers.kotya.home}/.Xauthority";
        };
        serviceConfig = {
            PIDFile = "/var/run/xkeysnail.pid";
            Restart = "always";
            RestartSec = 1;
            ExecStart = "${pkgs.xkeysnail}/bin/xkeysnail ${config.users.extraUsers.kotya.home}/.config/xkeysnail/config.py";
        };
    };
}
