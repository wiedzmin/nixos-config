{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.workstation.power.battery; # TODO: consider checking if it is a laptop, or any similar check
  user = config.attributes.mainUser.name;
in {
  options = {
    workstation.power.battery = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable laptop battery management";
      };
      device = mkOption {
        type = types.str;
        default = "BAT0";
        description = "Battery to monitor.";
      };
      dischargeNotificationPercents = mkOption {
        type = types.int;
        default = 10;
        description = "Battery level below which notifications should be sent.";
      };
      suspension.percents = mkOption {
        type = types.int;
        default = 5;
        description = "Battery level below which a suspend should be performed.";
      };
      suspension.timeout = mkOption {
        type = types.str;
        default = "60s";
        description = "How much to wait before suspending.";
      };
      suspension.command = mkOption {
        type = types.str;
        default = "systemctl suspend";
        description = "Command to use for suspending.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      systemd.user.timers."lowbatt" = renderTimer "check battery level" "1m" "1m" "";
      systemd.user.services."lowbatt" = {
        description = "battery level notifier";
        serviceConfig.PassEnvironment = "DISPLAY";
        script = ''
          export battery_capacity=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.device}/capacity)
          export battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.device}/status)
          if [[ $battery_capacity -le ${
            builtins.toString cfg.dischargeNotificationPercents
          } && $battery_status = "Discharging" ]]; then
              ${pkgs.dunst}/bin/dunstify -u critical "Battery low, consider plugging in."
          fi

          if [[ $battery_capacity -le ${
            builtins.toString cfg.suspension.percents
          } && $battery_status = "Discharging" ]]; then
              ${pkgs.dunst}/bin/dunstify -u critical -t 5000 "Battery CRITICALLY low, will suspend in ${cfg.suspension.timeout}."
              sleep ${cfg.suspension.timeout}

              battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.device}/status)
              if [[ $battery_status = "Discharging" ]]; then
                  ${cfg.suspension.command}
              fi
          fi
        '';
      };
    })
  ];
}
