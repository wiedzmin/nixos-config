{ config, lib, pkgs, ...}:
with lib;

let
    cfg = config.services.sshuttle;
in {
    options = {
        services.sshuttle = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable tunneling service.
                '';
            };
            remote = mkOption {
                type = types.nullOr types.str;
                default = null;
                example = "root@example.com";
                description = ''
                    remote sshuttle server identity.
                '';
            };
            excludeSubnets = mkOption {
                type = types.listOf types.str;
                default = [ ];
                description = ''
                    Route these subnets as normal.
                '';
            };
            sshIdentity = mkOption {
                type = types.str;
                default = "/home/${userName}/.ssh/id_rsa";
                description = "SSH identity file";
            };
            # TODO: if disabled, find out how to turn off service before suspend
            keep = mkOption {
                type = types.bool;
                default = false;
                description = ''
                    Whether to keep sshuttle up and running between suspends.
                '';
            };
        };
    };

    config = mkIf cfg.enable {
        assertions = [
            { assertion = cfg.remote != null; message = "Must provide remote identity."; }
        ];

        systemd.user.services."sshuttle" = {
            description = "sshuttle tunnel to remote server";
            after = [ "network.target" "suspend.target" ];
            path = [ pkgs.logger ];
            serviceConfig = {
                Type = "forking";
                Restart = "always";
                RestartSec = 2;
                ExecStart = "-/run/wrappers/bin/sudo ${pkgs.sshuttle}/bin/sshuttle -D --dns -r ${cfg.remote}" +
                            (lib.concatMapStrings (subnet: " -x ${subnet}") cfg.excludeSubnets) +
                            " 0/0 --pidfile=/var/run/sshuttle.pid -e 'ssh -i ${cfg.sshIdentity}'";
                ExecStop = "/run/wrappers/bin/sudo ${pkgs.procps}/bin/pkill -SIGTERM -f sshuttle";
                KillMode = "mixed";
            };
        };

        systemd.user.services."keep-sshuttle" = mkIf cfg.keep {
            description = "Restart Sshuttle after suspend";
            after = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
            partOf = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ]; # check if it needed/useful
            wantedBy = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
            serviceConfig = {
                Type = "oneshot";
                ExecStart = "${pkgs.systemd}/bin/systemctl --user try-restart sshuttle.service";
            };
        };
    };
}
