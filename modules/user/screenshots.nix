{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.screenshots;
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
  screenshot_active_window = pkgs.writeShellScriptBin "screenshot_active_window" ''
    ${pkgs.maim}/bin/maim -o -i $(${pkgs.xdotool}/bin/xdotool getactivewindow) --format png /dev/stdout | \
        ${pkgs.coreutils}/bin/tee ${cfg.baseDir}/screenshot-$(date ${cfg.dateFormat}.png | ${pkgs.coreutils}/bin/tr -d '[:cntrl:]') | \
        ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
  '';
  screenshot_full = pkgs.writeShellScriptBin "screenshot_full" ''
    ${pkgs.maim}/bin/maim -o --format png /dev/stdout | \
        ${pkgs.coreutils}/bin/tee ${cfg.baseDir}/screenshot-$(date ${cfg.dateFormat}.png | ${pkgs.coreutils}/bin/tr -d '[:cntrl:]') | \
        ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
  '';
  screenshot_region = pkgs.writeShellScriptBin "screenshot_region" ''
    ${pkgs.maim}/bin/maim -o -s --format png /dev/stdout | \
        ${pkgs.coreutils}/bin/tee ${cfg.baseDir}/screenshot-$(date ${cfg.dateFormat}.png | ${pkgs.coreutils}/bin/tr -d '[:cntrl:]') | \
        ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
  '';
in {
  options = {
    screenshots = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable screenshots functionality";
      };
      baseDir = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Screenshots base directory";
      };
      dateFormat = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "screenshot date suffix format";
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
        assertion = cfg.dateFormat != null;
        message = "Must provide date format.";
      }
      {
        assertion = (cfg.bootTimespec == "" && cfg.activeTimespec == "" && cfg.calendarTimespec != "")
          || (cfg.bootTimespec != "" && cfg.activeTimespec != "" && cfg.calendarTimespec == "");
        message = "screenshots: Must provide either calendarTimespec or bootTimespec/activeTimespec pair.";
      }
    ];

    environment.systemPackages = [
      order_screenshots
      screenshot_active_window
      screenshot_full
      screenshot_region
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
