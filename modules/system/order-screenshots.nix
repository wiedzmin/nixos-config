{ config, lib, pkgs, ... }:
with lib;

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
        type = types.nullOr types.str;
        default = null;
        example = "$HOME/screenshots";
        description = ''
          Screenshots base directory.
        '';
      };
      bootTimespec = mkOption {
        type = types.str;
        default = "";
        description = ''
          Interval to activate service after system boot (in systemd format).
        '';
      };
      activeTimespec = mkOption {
        type = types.str;
        default = "";
        description = ''
          Interval to activate service while system runs (in systemd format).
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
        assertion = cfg.baseDir != null;
        message = "Must provide path to screenshots dir.";
      }
      {
        assertion = (cfg.bootTimespec == "" && cfg.activeTimespec == "" && cfg.calendarTimespec != "")
          || (cfg.bootTimespec != "" && cfg.activeTimespec != "" && cfg.calendarTimespec == "");
        message = "order-screenshots: Must provide either calendarTimespec or bootTimespec/activeTimespec pair.";
      }
    ];

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
    systemd.user.timers."order-screenshots" = {
      description = "Screenshots ordering";
      wantedBy = [ "timers.target" ];
      timerConfig = if (cfg.bootTimespec != "") then {
        OnBootSec = cfg.bootTimespec;
        OnUnitActiveSec = cfg.activeTimespec;
      } else {
        OnCalendar = cfg.calendarTimespec;
      };
    };
  };
}
