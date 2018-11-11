{config, pkgs, lib, ...}:

{
    systemd.services."sshuttle" = {
        enable = true;
        description = "sshuttle tunnel to remote server";
        after = [ "network.target" "suspend.target" ];
        wantedBy = [ "graphical-session.target" ];
        path = [ pkgs.sshuttle pkgs.logger ];
        serviceConfig = {
            Type = "forking";
            Restart = "always";
            RestartSec = 2;
            ExecStart = "-${pkgs.sshuttle}/bin/sshuttle -D --dns -r ${config.sshuttle_traits.remote}" +
                        (lib.concatMapStrings (subnet: " -x ${subnet}") config.sshuttle_traits.exclude) +
                        " 0/0 --pidfile=/var/run/sshuttle.pid -e 'ssh -i ${config.sshuttle_traits.ssh_identity_file}'";
            ExecStartPost = "${pkgs.systemd}/bin/systemctl try-restart imapfilter.service"; # TODO: invent a global list of services that needs to be restarted this way
            ExecStopPost = "${pkgs.systemd}/bin/systemctl try-restart imapfilter.service";
        };
    };
}
