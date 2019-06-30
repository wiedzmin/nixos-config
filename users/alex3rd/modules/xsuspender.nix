{ config, lib, pkgs, ...}:
with import ../../../pkgs/util.nix {inherit config pkgs lib;};
with import ../const.nix {inherit config pkgs;};
with lib;

let
    cfg = config.services.xsuspender;
in {
    options = {
        services.xsuspender = {
            enable = mkOption {
                type = types.bool;
                default = false;
                description = ''
                    Whether to enable XSuspender.
                '';
            };

            debug = mkOption {
                description = "Whether to enable debug output.";
                type = types.bool;
                default = false;
            };
            # TODO: think out and add option(s) for udev rules
        };
    };

    config = mkIf cfg.enable {
        # TODO: add udev rules for dock/undock
        # TODO: add udev rules for AC/DC
        systemd.user.services."xsuspender" = {
            description = "Xsuspender";
            after = [ "graphical-session-pre.target" ];
            partOf = [ "graphical-session.target" ];
            wantedBy = [ "graphical-session.target" ];
            environment = mkIf cfg.debug {
                G_MESSAGE_DEBUG = "all";
            };
            serviceConfig = {
                PIDFile = "/run/xsuspender.pid";
                Restart = "always";
                RestartSec = 1;
                ExecStart = "${pkgs.xsuspender}/bin/xsuspender";
            };
            restartTriggers = [
                "/home/${userName}/.config/xsuspender.conf"
            ];
        };
    };
}
