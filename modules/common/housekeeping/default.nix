{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.custom.housekeeping;
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
      metadataCacheInstructions = mkOption {
        type = types.lines;
        default = "";
        description = "Set of commands needed to initialize system-wide data cache.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      services.redis.enable = true; # for various caching needs

      systemd.services.redis.postStart = cfg.metadataCacheInstructions;
    })
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
      systemd.user.timers."clean-trash" = renderTimer "Clean trash" "" "" cfg.cleanTrash.calendarTimespec;
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
      systemd.user.timers."purge-home-cache" = renderTimer "Purge homedir cache" "" "" cfg.purgeExpired.calendarTimespec;
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
      systemd.user.timers."purge-temp-files" = renderTimer "Purge temporary files" "" "" cfg.purgeExpired.calendarTimespec;
    })
    (mkIf (cfg.enable && cfg.orderScreenshots.enable) {
      assertions = [{
        assertion = (cfg.orderScreenshots.enable && config.custom.content.screenshots.enable);
        message = "housekeeping: it makes no sense to order screenshot without enabling making them first.";
      }];

      nixpkgs.config.packageOverrides = _: rec {
        order_screenshots = pkgs.writeShellScriptBin "order_screenshots" (builtins.readFile (pkgs.substituteAll
          ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./order_screenshots.sh; })));
      };

      systemd.user.services."order-screenshots" = {
        description = "Screenshots ordering";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.order_screenshots}/bin/order_screenshots";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."order-screenshots" = renderTimer "Screenshots ordering" "" "" cfg.orderScreenshots.calendarTimespec;
    })
    (mkIf cfg.fsDeduplication.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ dupd jdupes rmlint fpart ];
      };
    })
  ];
}
