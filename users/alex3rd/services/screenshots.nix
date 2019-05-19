{config, pkgs, lib, ...}:
with import ../const.nix {inherit config pkgs;};
{
    systemd.services."order-screenshots" = let
        order_screenshots = pkgs.writeShellScriptBin "order_screenshots" ''
            declare -A REGEXP_TO_DATECMD
            REGEXP_TO_DATECMD=(
              ["screenshot-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}_[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:11:4} ''${FILENAME:16:2} ''${FILENAME:19:2}'
              ["screenshot-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:11:4} ''${FILENAME:16:2} ''${FILENAME:19:2}'
              ["[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}_[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}_[0-9]\+x[0-9]\+_scrot"]='echo ''${FILENAME:0:4} ''${FILENAME:5:2} ''${FILENAME:8:2}'
              ["[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{6\}_[0-9]\+x[0-9]\+_scrot"]='echo ''${FILENAME:0:4} ''${FILENAME:5:2} ''${FILENAME:8:2}'
              ["screenshot-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{4\}-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:17:4} ''${FILENAME:14:2} ''${FILENAME:11:2}'
              # ["screenshot-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}\\ [0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}"]='echo ''${FILENAME:20:4} ''${FILENAME:25:2} ''${FILENAME:28:2}'
              ["screenshot-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}_[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}"]='echo ''${FILENAME:20:4} ''${FILENAME:25:2} ''${FILENAME:28:2}'
            )
            CONFIGFILE=''${1:-$HOME/.config/screenshots/screenshots.yml}
            SCREENSHOTS_PATH=$(${pkgs.shyaml}/bin/shyaml -gy screenshots.path $CONFIGFILE)

            for regexp in "''${!REGEXP_TO_DATECMD[@]}"
            do
                FILELIST=$(ls -a $SCREENSHOTS_PATH | grep -e $regexp)
                DATECMD=''${REGEXP_TO_DATECMD[$regexp]}
                for FILENAME in $FILELIST
                do
                    DATE_PART=($(eval $DATECMD))
                    YEAR=''${DATE_PART[0]}
                    MONTH=''${DATE_PART[1]}
                    DAY=''${DATE_PART[2]}
                    DEST_PATH=$SCREENSHOTS_PATH/$YEAR/$MONTH/$DAY
                    mkdir -p $SCREENSHOTS_PATH/$YEAR/$MONTH/$DAY
                    echo "moving $FILENAME to $DEST_PATH"
                    mv $SCREENSHOTS_PATH/$FILENAME $SCREENSHOTS_PATH/$YEAR/$MONTH/$DAY
                done
            done

            exit 0
        '';
    in
    {
        description = "Timely order screenshots";
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
            ExecStart = "${order_screenshots}";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."order-screenshots" = {
        description = "Timely order screenshots";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "5min";
            OnUnitActiveSec = "1d";
        };
    };
}
