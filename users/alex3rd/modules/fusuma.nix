{ config, lib, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
with lib;

# TODO: think of adding more options
let
    cfg = config.services.fusuma;
in {
    options = {
        services.fusuma = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable fusuma input method.
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
            systemd.user.services."fusuma" = {
                description = "Fusuma input method";
                wantedBy = [ "graphical.target" ];
                partOf = [ "graphical.target" ];
                environment = {
                    DISPLAY = ":0";
                    XAUTHORITY = "/home/${cfg.userName}/.Xauthority";
                };
                serviceConfig = {
                    PIDFile = "/var/run/fusuma.pid";
                    Restart = "always";
                    RestartSec = 1;
                    ExecStart = "${pkgs.fusuma}/bin/fusuma -c ${cfg.userName}/.config/fusuma/config.yml";
                };
            };
            home-manager.users."${cfg.userName}" = {
                home.file = {
                    ".config/fusuma/config.yml".text = builtins.toJSON { # TODO: maybe extract some parameters from below
                        "swipe" = {
                            "3" = {
                                "left" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key alt+.";
                                };
                                "right" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key alt+,";
                                };
                                "up" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key ctrl+t";
                                    "threshold" = "1.5";
                                };
                                "down" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key ctrl+w";
                                    "threshold" = "1.5";
                                };
                            };
                            "4" = {
                                "left" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key super+Left";
                                };
                                "right" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key super+Right";
                                };
                                "up" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key super+a";
                                };
                                "down" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key super+s";
                                };
                            };
                        };
                        "pinch" = {
                            "2" = {
                                "in" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key ctrl+plus";
                                    "threshold" = "0.1";
                                };
                                "out" = {
                                    "command" = "${pkgs.xdotool}/bin/xdotool key ctrl+minus";
                                    "threshold" = "0.1";
                                };
                            };
                        };
                        "threshold" = {
                            "swipe" = "1";
                            "pinch" = "1";
                        };
                        "interval" = {
                            "swipe" = "1";
                            "pinch" = "1";
                        };
                    };
                };
            };
        })
    ];
}
