{config, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
{
    systemd.services."xsuspender" = {
        enable = true;
        description = "XSuspender";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        environment = {
            DISPLAY = ":0";
            XAUTHORITY = "/home/${userName}/.Xauthority";
        };
        serviceConfig = {
            User = "${userName}";
            PIDFile = "/var/run/xsuspender.pid";
            Restart = "always";
            RestartSec = 1;
            ExecStart = "${pkgs.xsuspender}/bin/xsuspender";
        };
    };
}
