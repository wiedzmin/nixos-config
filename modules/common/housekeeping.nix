{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.housekeeping;
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
      FILELIST=$(ls -a ${config.custom.content.screenshots.baseDir} | grep -e $regexp)
      DATECMD=''${REGEXP_TO_DATECMD[$regexp]}
      for FILENAME in $FILELIST
      do
        DATE_PART=($(eval $DATECMD))
        YEAR=''${DATE_PART[0]}
        MONTH=''${DATE_PART[1]}
        DAY=''${DATE_PART[2]}
        DEST_PATH=${config.custom.content.screenshots.baseDir}/$YEAR/$MONTH/$DAY
        mkdir -p ${config.custom.content.screenshots.baseDir}/$YEAR/$MONTH/$DAY
        echo "moving $FILENAME to $DEST_PATH"
        mv ${config.custom.content.screenshots.baseDir}/$FILENAME ${config.custom.content.screenshots.baseDir}/$YEAR/$MONTH/$DAY
      done
    done

    exit 0
 '';
in {
  options = {
    custom.housekeeping = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable automated 'housekeeping'.";
      };
      cleanTrash.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable trash cleaning.";
      };
      cleanTrash.emptyInterval = mkOption {
        type = types.int;
        default = 7;
        description = "Days to keep trash.";
      };
      cleanTrash.calendarTimespec = mkOption {
        type = types.str;
        default = "";
        description = "Timestamp of service activation (in systemd format).";
      };
      healthChecking.enable = mkOption { # periodically checking systemd services journals for errors
        type = types.bool;
        default = false;
        description = "Whether to enable systemd service healthchecking.";
      };
      purgeExpired.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable $HOME/.cache and $HOME/.config
          temporary files cleaning.
        '';
      };
      purgeExpired.cacheDepth = mkOption {
        type = types.str;
        default = "";
        example = "7d";
        description = "Time delta to consider cache files being older expired.";
      };
      purgeExpired.tempDepth = mkOption {
        type = types.str;
        default = "";
        example = "30d";
        description = "Time delta to consider temporary files being older expired.";
      };
      purgeExpired.calendarTimespec = mkOption {
        type = types.str;
        default = "";
        description = "Timestamp of service activation (in systemd format).";
      };
      orderScreenshots.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable screenshots ordering.";
      };
      orderScreenshots.calendarTimespec = mkOption {
        type = types.str;
        default = "";
        description = "Timestamp of service activation (in systemd format).";
      };
      fsDeduplication.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable FS deduplication tools.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.cleanTrash.enable) {
      assertions = [
        {
          assertion = (cfg.cleanTrash.enable && cfg.cleanTrash.calendarTimespec != "");
          message = "housekeeping: must schedule trash cleaning once it was enabled.";
        }
        {
          assertion = (!cfg.healthChecking.enable);
          message = "housekeeping: healthchecks are not implemented yet.";
        }
      ];

      systemd.user.services."clean-trash" = {
        description = "Clean trash";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.trash-cli}/bin/trash-empty ${builtins.toString cfg.cleanTrash.emptyInterval}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."clean-trash" = {
        description = "Clean trash";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfg.cleanTrash.calendarTimespec;
        };
      };
    })
    (mkIf (cfg.enable && cfg.purgeExpired.enable) {
      assertions = [{
        assertion = (cfg.purgeExpired.enable && cfg.cleanTrash.calendarTimespec != "");
        message = "housekeeping: must schedule trash cleaning once it was enabled.";
      }];

      systemd.user.services."purge-home-cache" = {
        description = "Purge homedir cache";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = ''
            ${pkgs.fd}/bin/fd --no-ignore \
                              --changed-before ${purgeExpired.cacheDepth} \
                              . /home/${config.attributes.mainUser.name}/.cache \
                              --exec rm -f {}
          '';
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."purge-home-cache" = {
        description = "Purge homedir cache";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfg.purgeExpired.calendarTimespec;
        };
      };
      systemd.user.services."purge-temp-files" = {
        description = "Purge temporary files";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = ''
            ${pkgs.fd}/bin/fd --no-ignore \
                              --changed-before ${purgeExpired.tempDepth} \
                              --type f --type e \
                              . /home/${config.attributes.mainUser.name}/.config \
                              --exec ${pkgs.trash-cli}/bin/trash-put {}
          '';
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."purge-temp-files" = {
        description = "Purge temporary files";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfg.purgeExpired.calendarTimespec;
        };
      };
    })
    (mkIf (cfg.enable && cfg.orderScreenshots.enable) {
      assertions = [
        {
          assertion = (cfg.orderScreenshots.enable && config.custom.content.screenshots.enable);
          message = "housekeeping: it makes no sense to order screenshot without enabling making them first.";
        }
      ];

      systemd.user.services."order-screenshots" = {
        description = "Screenshots ordering";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${order_screenshots}/bin/order_screenshots";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."order-screenshots" = {
        description = "Screenshots ordering";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfg.orderScreenshots.calendarTimespec;
        };
      };
    })
    (mkIf cfg.fsDeduplication.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          dupd
          jdupes
          rmlint
          fpart
        ];
      };
    })
  ];
}
