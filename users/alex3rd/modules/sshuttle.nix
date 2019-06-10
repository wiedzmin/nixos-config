{ config, lib, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
with lib;

# TODO: think of adding more options
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
                type = types.str;
                default = "";
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
        };
    };

    config = mkMerge [
        {
            assertions = [ # FIXME: assertion condition fires before this at "compile" time
                { assertion = cfg.remote != ""; message = "Must provide remote identity."; }
                { assertion = cfg.sshIdentity != ""; message = "Must provide local identity."; }
            ];
        }

        (mkIf cfg.enable {
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
                };
            };
        })
    ];
}
