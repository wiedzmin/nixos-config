{ config, lib, pkgs, ...}:
with lib;

let
    cfg = config.services.xkeysnail;
in {
    options = {
        services.xkeysnail = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable xkeysnail.
                '';
            };
            configFile = mkOption {
                type = types.str;
                default = "";
                description = ''
                    Config file absolute path.
                '';
            };
            inputDevices = mkOption {
                type = types.listOf types.str;
                default = [ ];
                example = literalExample ''
                    [
                        "/dev/input/event3"
                    ]
                '';
                description = ''
                    Keyboard devices to remap (if omitted,
                    xkeysnail will choose proper keyboard
                    devices)
                '';
            };
        };
    };

    config = mkIf cfg.enable {
        assertions = [
            { assertion = cfg.configFile != ""; message = "XKeysnail: must provide config file path."; }
        ];

        systemd.user.services."xkeysnail" = {
            description = "Xkeysnail";
            after = [ "graphical-session-pre.target" ];
            partOf = [ "graphical-session.target" ];
            wantedBy = [ "graphical-session.target" ];
            serviceConfig = {
                PIDFile = "/run/xkeysnail.pid";
                Restart = "always";
                RestartSec = 1;
                ExecStart = "/run/wrappers/bin/sudo ${pkgs.xkeysnail}/bin/xkeysnail ${optionalString
                            (cfg.inputDevices != []) "--devices ${lib.concatStringsSep " " cfg.inputDevices}"} ${cfg.configFile}";
            };
        };
    };
}
