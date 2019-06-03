{config, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
{
    systemd.services."fusuma" = {
        enable = true;
        description = "Fusuma";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        environment = {
            DISPLAY = ":0";
            XAUTHORITY = "/home/${userName}/.Xauthority";
        };
        path = with pkgs; [ xdotool ];
        serviceConfig = {
            User = "${userName}";
            PIDFile = "/var/run/fusuma.pid";
            Restart = "always";
            RestartSec = 1;
            ExecStart = "${pkgs.fusuma}/bin/fusuma -c ${userName}/.config/fusuma/config.yml";
        };
    };
}
