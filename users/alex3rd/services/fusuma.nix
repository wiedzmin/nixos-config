{config, pkgs, ...}:

{
    systemd.services."fusuma" = {
        enable = true;
        description = "Fusuma";
        wantedBy = [ "graphical.target" ];
        environment = {
            DISPLAY = ":0";
            XAUTHORITY = "${config.users.extraUsers.alex3rd.home}/.Xauthority";
        };
        serviceConfig = {
            User = "alex3rd"; # TODO: think of abstracting away
            PIDFile = "/var/run/fusuma.pid";
            Restart = "always";
            RestartSec = 1;
            ExecStart = "${pkgs.fusuma}/bin/fusuma -c ${config.users.extraUsers.alex3rd.home}/.config/fusuma/config.yml";
        };
    };
}
