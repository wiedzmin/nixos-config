{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.localinfra.powermanagement;
  user = config.attributes.mainUser.name;
in {
  options = {
    localinfra.powermanagement = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable customized power management.";
      };
      resumeCommands = mkOption {
        type = types.lines;
        default = "";
        description = "Commands to perform on wakeup.";
      };
      powerDownCommands = mkOption {
        type = types.lines;
        default = "";
        description = "Commands to perform on system shutdown.";
      };
      batteryManagement.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable battery status controlling.";
      };
      batteryManagement.device = mkOption {
        type = types.str;
        default = "BAT0";
        description = "Battery to monitor.";
      };
      batteryManagement.notificationThreshold = mkOption {
        type = types.int;
        default = 10;
        description = "Battery level below which notifications should be sent.";
      };
      batteryManagement.suspensionThreshold = mkOption {
        type = types.int;
        default = 5;
        description = "Battery level below which a suspend should be performed.";
      };
      batteryManagement.suspendTimeout = mkOption {
        type = types.str;
        default = "60s";
        description = "How much to wait before suspending.";
      };
      batteryManagement.suspendCommand = mkOption {
        type = types.str;
        default = "systemctl suspend";
        description = "Command to issue on suspend.";
      };
      appsSuspension.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable apps suspension.";
      };
      appsSuspension.rules = mkOption {
        type = types.attrs;
        default = {
          Chromium = {
            suspendDelay = 10;
            matchWmClassContains = "Chromium-browser";
            suspendSubtreePattern = "chromium";
          };
          Firefox = {
            suspendDelay = 10;
            matchWmClassContains = "Firefox";
            suspendSubtreePattern = "firefox";
          };
        };
        description = "Apps suspending rules.";
      };
      warmup.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable pulling some highly used data into RAM.";
      };
      warmup.paths = mkOption {
        type =  types.listOf types.str;
        default = [ ];
        description = "Paths to pull.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      powerManagement = {
        enable = true;
        powertop.enable = true;
        resumeCommands = cfg.resumeCommands;
      };
      services.upower.enable = true;
      services.tuptime.enable = true;
    })
    (mkIf cfg.appsSuspension.enable {
      home-manager.users.${user} = {
        services.xsuspender = {
          enable = true;
          defaults = {
            suspendDelay = 10;
            onlyOnBattery = false;
          };
          rules = cfg.appsSuspension.rules;
        };
      };
    })
    (mkIf (cfg.wm.enable && cfg.appsSuspension.enable) {
      wmCommon.keys = [
        {
          key = [ "x" ];
          cmd = "${pkgs.systemd}/bin/systemctl --user restart xsuspender.service";
          mode = "service";
        }
        {
          key = [ "Shift" "x" ];
          cmd = "${pkgs.systemd}/bin/systemctl --user stop xsuspender.service";
          mode = "service";
        }
      ];
    })
    (mkIf (cfg.batteryManagement.enable) {
      systemd.user.timers."lowbatt" = renderTimer "check battery level" "1m" "1m" "";
      systemd.user.services."lowbatt" = {
        description = "battery level notifier";
        serviceConfig.PassEnvironment = "DISPLAY";
        script = ''
          export battery_capacity=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.batteryManagement.device}/capacity)
          export battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.batteryManagement.device}/status)
          if [[ $battery_capacity -le ${
            builtins.toString cfg.batteryManagement.notificationThreshold
          } && $battery_status = "Discharging" ]]; then
              ${pkgs.dunst}/bin/dunstify -u critical "Battery low, consider plugging in."
          fi

          if [[ $battery_capacity -le ${
            builtins.toString cfg.batteryManagement.suspensionThreshold
          } && $battery_status = "Discharging" ]]; then
              ${pkgs.dunst}/bin/dunstify -u critical -t 5000 "Battery CRITICALLY low, will suspend in ${cfg.batteryManagement.suspendTimeout}."
              sleep ${cfg.batteryManagement.suspendTimeout}

              battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.batteryManagement.device}/status)
              if [[ $battery_status = "Discharging" ]]; then
                  ${cfg.batteryManagement.suspendCommand}
              fi
          fi
        '';
      };
    })
    (mkIf (cfg.warmup.enable && cfg.warmup.paths != [ ]) {
      systemd.user.services."warmup" = {
        description = "Warm up paths";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.vmtouch}/bin/vmtouch -t ${lib.concatStringsSep " " cfg.warmup.paths}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
      };
    })
  ];
}
