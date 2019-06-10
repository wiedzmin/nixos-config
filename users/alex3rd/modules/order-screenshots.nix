{ config, lib, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
with lib;

# TODO: think of adding more options
let
    cfg = config.services.order-screenshots;

    order_screenshots = pkgs.writeShellScriptBin "order_screenshots" ''
        declare -A REGEXP_TO_DATECMD
        REGEXP_TO_DATECMD=(
          ["screenshot-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}_[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:11:4} ''${FILENAME:16:2} ''${FILENAME:19:2}'
          ["screenshot-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:11:4} ''${FILENAME:16:2} ''${FILENAME:19:2}'
          ["[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}_[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}_[0-9]\+x[0-9]\+_scrot"]='echo ''${FILENAME:0:4} ''${FILENAME:5:2} ''${FILENAME:8:2}'
          ["[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{6\}_[0-9]\+x[0-9]\+_scrot"]='echo ''${FILENAME:0:4} ''${FILENAME:5:2} ''${FILENAME:8:2}'
          ["screenshot-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{4\}-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:17:4} ''${FILENAME:14:2} ''${FILENAME:11:2}'
          ["screenshot-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}\\ [0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}"]='echo ''${FILENAME:20:4} ''${FILENAME:25:2} ''${FILENAME:28:2}'
          ["screenshot-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}_[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}"]='echo ''${FILENAME:20:4} ''${FILENAME:25:2} ''${FILENAME:28:2}'
        )

        for regexp in "''${!REGEXP_TO_DATECMD[@]}"
        do
            FILELIST=$(ls -a ${cfg.baseDir} | grep -e $regexp)
            DATECMD=''${REGEXP_TO_DATECMD[$regexp]}
            for FILENAME in $FILELIST
            do
                DATE_PART=($(eval $DATECMD))
                YEAR=''${DATE_PART[0]}
                MONTH=''${DATE_PART[1]}
                DAY=''${DATE_PART[2]}
                DEST_PATH=${cfg.baseDir}/$YEAR/$MONTH/$DAY
                mkdir -p ${cfg.baseDir}/$YEAR/$MONTH/$DAY
                echo "moving $FILENAME to $DEST_PATH"
                mv ${cfg.baseDir}/$FILENAME ${cfg.baseDir}/$YEAR/$MONTH/$DAY
            done
        done

        exit 0
    '';
in {
    options = {
        services.order-screenshots = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable screenshots ordering.
                '';
            };
            baseDir = mkOption {
                type = types.str;
                default = "";
                example = "$HOME/screenshots";
                description = ''
                    Screenshots base directory.
                '';
            };
        };
    };

    config = mkMerge [
        {
            assertions = [ # FIXME: assertion condition fires before this at "compile" time
                { assertion = cfg.baseDir != ""; message = "Must provide path to screenshots dir."; }
            ];
        }

        (mkIf cfg.enable {
            systemd.user.services."order-screenshots" = {
                description = "Screenshots ordering";
                wantedBy = [ "graphical.target" ];
                partOf = [ "graphical.target" ];
                serviceConfig = {
                    Type = "oneshot";
                    ExecStart = "${order_screenshots}";
                    StandardOutput = "journal+console";
                    StandardError = "inherit";
                };
            };
            systemd.timers."order-screenshots" = {
                description = "Screenshots ordering";
                wantedBy = [ "timers.target" ];
                timerConfig = {
                    OnBootSec = "5min";
                    OnUnitActiveSec = "1d"; # TODO: use onCalendar
                };
            };
        })
    ];
}
