{config, pkgs, lib, ...}:

{
    imports = [
        ../private/network.nix
    ];
    systemd.user.services."sshuttle" = {
        enable = true;
        description = "sshuttle tunnel to remote server";
        after = [ "network.target" "suspend.target" ];
        wantedBy = [ "graphical-session.target" ];
        path = [ pkgs.logger ];
        serviceConfig = {
            Type = "forking";
            Restart = "always";
            RestartSec = 2;
            ExecStart = "-/run/wrappers/bin/sudo ${pkgs.sshuttle}/bin/sshuttle -D --dns -r ${config.network.sshuttle.remote}" +
                        (lib.concatMapStrings (subnet: " -x ${subnet}") config.network.sshuttle.exclude) +
                        " 0/0 --pidfile=/var/run/sshuttle.pid -e 'ssh -i ${config.network.sshuttle.sshIdentityFile}'";
            ExecStartPost = "${pkgs.systemd}/bin/systemctl try-restart imapfilter.service"; # TODO: invent a global list of services that needs to be restarted this way
            ExecStopPost = "${pkgs.systemd}/bin/systemctl try-restart imapfilter.service";
        };
    };
}
