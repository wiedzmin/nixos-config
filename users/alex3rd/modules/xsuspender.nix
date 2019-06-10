{ config, lib, pkgs, ...}:
with import ../../../util.nix {inherit config pkgs lib;};
with import ../const.nix {inherit config pkgs;};
with lib;

# TODO: think of adding more options
let
    cfg = config.services.xsuspender;
in {
    options = {
        services.xsuspender = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable xsuspender.
                '';
            };
            userName = mkOption {
                type = types.str;
                default = "";
                description = ''
                    Name of a user who runs service.
                '';
            };
        };
    };

    config = mkMerge [
        {
            assertions = [ # FIXME: assertion condition fires before this at "compile" time
                { assertion = cfg.userName != ""; message = "Must provide user name."; }
            ];
        }

        (mkIf cfg.enable {
            systemd.user.services."xsuspender" = {
                description = "Xsuspender";
                wantedBy = [ "graphical.target" ];
                partOf = [ "graphical.target" ];
                environment = {
                    DISPLAY = ":0";
                    XAUTHORITY = "/home/${userName}/.Xauthority";
                };
                serviceConfig = {
                    PIDFile = "/var/run/xsuspender.pid";
                    Restart = "always";
                    RestartSec = 1;
                    ExecStart = "${pkgs.xsuspender}/bin/xsuspender";
                };
            };
            home-manager.users."${cfg.userName}" = {
                home.file = {
                    ".config/xsuspender.conf".text = genIni {
                        Default = {
                            suspend_delay = 10;
                            resume_every = 50;
                            resume_for = 5;
                            only_on_battery = true;
                            auto_suspend_on_battery = true;
                            send_signals = true;
                        };
                        VirtualBox = {
                            match_wm_class_contains = "VirtualBox";
                            send_signals = false;
                            exec_suspend = ''VBoxManage controlvm "$(ps -o args= -q $PID | sed -E ’s/.*--startvm ([a-f0-9-]+).*/\1/’)" pause'';
                            exec_resume  = ''VBoxManage controlvm "$(ps -o args= -q $PID | sed -E ’s/.*--startvm ([a-f0-9-]+).*/\1/’)" resume'';
                        };
                        Firefox = {
                            match_wm_class_contains = "Firefox";
                        };
                        Chromium = {
                            match_wm_class_contains = "Chromium-browser";
                        };
                    };
                };
            };
        })
    ];
}
