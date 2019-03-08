{config, pkgs, ...}:

{
    systemd.services."xsuspender" = {
        enable = true;
        description = "XSuspender";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        environment = {
            DISPLAY = ":0";
            XAUTHORITY = "${config.users.extraUsers.alex3rd.home}/.Xauthority";
        };
        serviceConfig = {
            User = "alex3rd"; # TODO: think of abstracting away
            PIDFile = "/var/run/xsuspender.pid";
            Restart = "always";
            RestartSec = 1;
            ExecStart = "${pkgs.xsuspender}/bin/xsuspender";
        };
    };
}
