{ config, lib, pkgs, ... }:
with lib;

let cfg = config.services.clean-trash;
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
    assertions = [{
      assertion = (cfg.bootTimespec == "" && cfg.activeTimespec == "" && cfg.calendarTimespec != "")
        || (cfg.bootTimespec != "" && cfg.activeTimespec != "" && cfg.calendarTimespec == "");
      message = "Clean trash: Must provide either calendarTimespec or bootTimespec/activeTimespec pair.";
    }];

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
      timerConfig = if (cfg.bootTimespec != "") then {
        OnBootSec = cfg.bootTimespec;
        OnUnitActiveSec = cfg.activeTimespec;
      } else {
        OnCalendar = cfg.calendarTimespec;
      };
    };
  };
}
