{config, pkgs, ...}:

{
    systemd.services."xkeysnail" = {
        enable = true;
        description = "Xkeysnail";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        environment = {
            DISPLAY = ":0";
            XAUTHORITY = "${config.users.extraUsers.alex3rd.home}/.Xauthority";
        };
        serviceConfig = {
            PIDFile = "/var/run/xkeysnail.pid";
            Restart = "always";
            RestartSec = 1;
            ExecStart = "${pkgs.xkeysnail}/bin/xkeysnail ${config.users.extraUsers.alex3rd.home}/.config/xkeysnail/config.py";
        };
    };
}
