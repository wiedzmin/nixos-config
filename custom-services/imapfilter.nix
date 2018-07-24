{pkgs, ...}:

{
    environment.etc."imapfilter_config.lua".source = "/etc/nixos/private/imapfilter_config.lua";
    systemd.services."imapfilter" = {
        enable = true;
        description = "Imapfilter";
        after = [ "network.target" "suspend.target" ];
        wantedBy = [ "graphical-session.target" ];
        path = [ pkgs.imapfilter pkgs.gnupg ];
        serviceConfig = {
            PIDFile = "/var/run/imapfilter.pid";
            Restart = "always";
            RestartSec = 2;
            User="alex3rd";
            ExecStart = "${pkgs.imapfilter}/bin/imapfilter -c /etc/imapfilter_config.lua -v";
        };
    };
}
