# https://gist.github.com/domenkozar/82886ee82efee623cdc0d19eb81c7fb7
{ config, lib, pkgs, ... }:

with lib;

let cfg = config.services.batteryNotifier;
in {
  options = {
    services.batteryNotifier = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable battery notifier.
        '';
      };
      device = mkOption {
        type = types.str;
        default = "BAT0";
        description = ''
          Device to monitor.
        '';
      };
      notifyCapacity = mkOption {
        type = types.int;
        default = 10;
        description = ''
          Battery level at which a notification should be sent.
        '';
      };
      suspendCapacity = mkOption {
        type = types.int;
        default = 5;
        description = ''
          Battery level at which a suspend should be performed.
        '';
      };
      suspendTimeout = mkOption {
        type = types.str;
        default = "60s";
        description = ''
          How much to wait before suspending.
        '';
      };
      suspendCommand = mkOption {
        type = types.str;
        default = "systemctl suspend";
        description = ''
          Command to issue on suspend.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.timers."lowbatt" = {
      description = "check battery level";
      timerConfig.OnBootSec = "1m";
      timerConfig.OnUnitInactiveSec = "1m";
      timerConfig.Unit = "lowbatt.service";
      wantedBy = [ "timers.target" ];
    };
    systemd.user.services."lowbatt" = {
      description = "battery level notifier";
      serviceConfig.PassEnvironment = "DISPLAY";
      script = ''
        export battery_capacity=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.device}/capacity)
        export battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.device}/status)
        if [[ $battery_capacity -le ${builtins.toString cfg.notifyCapacity} && $battery_status = "Discharging" ]]; then
            ${pkgs.dunst}/bin/dunstify -u critical "Battery Low, you should probably plug-in."
        fi

        if [[ $battery_capacity -le ${builtins.toString cfg.suspendCapacity} && $battery_status = "Discharging" ]]; then
            ${pkgs.dunst}/bin/dunstify -u critical -t 5000 Battery CRITICALLY low, will suspend in ${cfg.suspendTimeout}."
            sleep ${cfg.suspendTimeout}

            battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.device}/status)
            if [[ $battery_status = "Discharging" ]]; then
                ${cfg.suspendCommand}
            fi
        fi
      '';
    };
  };
}
