{ config, lib, pkgs, ...}:
with lib;

let
    cfg = config.services.keynav;
in {
    options = {
        services.keynav = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable keynav.
                '';
            };
            # TODO: add config-related options/overrides
        };
    };

    config = mkIf cfg.enable {
        systemd.user.services."keynav" = {
            description = "Navigate mouse with keyboard";
            after = [ "graphical-session-pre.target" ];
            partOf = [ "graphical-session.target" ];
            wantedBy = [ "graphical-session.target" ];
            serviceConfig = {
                Type = "simple";
                PassEnvironment = "DISPLAY";
                ExecStart = ''${pkgs.keynav}/bin/keynav'';
            };
        };
    };
}
