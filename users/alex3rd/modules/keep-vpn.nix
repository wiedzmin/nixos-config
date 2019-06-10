{ config, lib, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
with lib;

# TODO: think of adding more options
let
    cfg = config.services.keep-vpn;
in {
    options = {
        services.keep-vpn = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable keeping vpn up and running.
                '';
            };
            vpnName = mkOption {
                type = types.str;
                default = "";
                description = ''
                    vpn service sub-name.
                '';
            };
        };
    };

    config = mkMerge [
        {
            assertions = [ # FIXME: assertion condition fires before this at "compile" time
                { assertion = cfg.vpnName != ""; message = "Must provide vpn service name."; }
            ];
        }

        (mkIf cfg.enable {
            systemd.services."keep-vpn" = {
                description = "Restart OpenVPN after suspend";
                after = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
                wantedBy = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
                script = ''
                  ${pkgs.systemd}/bin/systemctl try-restart openvpn-${cfg.vpnName}.service
                '';
            };
        })
    ];
}
