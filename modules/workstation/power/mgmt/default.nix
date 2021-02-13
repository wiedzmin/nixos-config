{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.workstation.power.mgmt;
in {
  options = {
    workstation.power.mgmt = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable sleep management.";
      };
      commands.resume = mkOption {
        type = types.lines;
        default = "";
        description = "Commands to perform on wakeup.";
      };
      commands.suspend = mkOption {
        type = types.lines;
        default = "";
        description = "Commands to perform on system shutdown.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      powerManagement = {
        enable = true;
        powertop.enable = true;
        resumeCommands = cfg.commands.resume;
        powerDownCommands = cfg.commands.suspend;
      };
      services = {
        auto-cpufreq.enable = true;
        upower.enable = true;
        tuptime.enable = true;
        tlp = {
          enable = true;
          settings = {
            START_CHARGE_THRESH_BAT0 = "80";
            STOP_CHARGE_THRESH_BAT0 = "90";
            DEVICES_TO_DISABLE_ON_WIFI_CONNECT = "wwan";
            USB_BLACKLIST_PHONE = 1;
          };
        };
      };
    })
  ];
}
