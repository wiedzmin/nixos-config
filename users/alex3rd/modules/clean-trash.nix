# trash-cli # misc # TODO: think of automating trash emptying https://github.com/andreafrancia/trash-cli
{ config, lib, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
with lib;

let
    cfg = config.services.clean-trash;
in {
    options = {
        services.clean-trash = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable automated trash cleaning.
                '';
            };
            emptyInterval = mkOption {
                type = types.int;
                default = 7;
                example = 14;
                description = ''
                    Days to keep trash.
                '';
            };
            calendarTimespec = mkOption {
                type = types.str;
                default = "";
                description = ''
                    Timestamp of service activation (in systemd format).
                '';
            };
        };
    };

    config = mkIf cfg.enable {
        assertions = [
            {
                assertion = cfg.calendarTimespec != "";
                message = "Must provide calendarTimespec.";
            }
        ];

        systemd.user.services."clean-trash" = {
            description = "Clean trash";
            serviceConfig = {
                Type = "oneshot";
                ExecStart = "${pkgs.trash-cli}/bin/trash-empty ${builtins.toString cfg.emptyInterval}";
                StandardOutput = "journal+console";
                StandardError = "inherit";
            };
        };
        systemd.user.timers."clean-trash" = {
            description = "Clean trash";
            wantedBy = [ "timers.target" ];
            timerConfig = {
                OnCalendar = cfg.calendarTimespec;
            };
        };
    };
}
