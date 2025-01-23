{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.gc;
  user = config.attributes.mainUser.name;
in
{
  options = {
    gc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable automated 'housekeeping'.";
      };
      expired.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable $HOME/.cache and $HOME/.config
          temporary files cleaning.
        '';
      };
      expired.cacheDepth = mkOption {
        type = types.str;
        default = "";
        example = "7d";
        description = "Time delta to consider cache files being older expired.";
      };
      expired.tempDepth = mkOption {
        type = types.str;
        default = "";
        example = "30d";
        description = "Time delta to consider temporary files being older expired.";
      };
      expired.calendarTimespec = mkOption {
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
    (mkIf (cfg.enable && cfg.expired.enable) {
      assertions = [{
        assertion = cfg.expired.enable && cfg.expired.calendarTimespec != "";
        message = "gc: must schedule trash cleaning once it was enabled.";
      }];

      systemd.user.services."purge-home-cache" = {
        description = "Purge homedir cache";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = ''
            ${pkgs.fd}/bin/fd --no-ignore \
                              --changed-before ${cfg.expired.cacheDepth} \
                              . ${homePrefix user ".cache"} \
                              --exec rm -f {}
          '';
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."purge-home-cache" =
        renderTimer "Purge homedir cache" "" "" cfg.expired.calendarTimespec false "";

      systemd.user.services."purge-temp-files" = {
        description = "Purge temporary files";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = ''
            ${pkgs.fd}/bin/fd --no-ignore \
                              --changed-before ${cfg.expired.tempDepth} \
                              --type f --type e \
                              . ${homePrefix user ".config"} \
                              --exec ${pkgs.trash-cli}/bin/trash-put {}
          '';
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."purge-temp-files" =
        renderTimer "Purge temporary files" "" "" cfg.expired.calendarTimespec false "";
    })
    (mkIf cfg.fsDeduplication.enable {
      home-manager.users."${user}" = { home.packages = with pkgs; [ dupd jdupes rmlint fpart czkawka ]; };
    })
  ];
}
