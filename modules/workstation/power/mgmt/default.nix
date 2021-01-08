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
      services.upower.enable = true;
      services.tuptime.enable = true;
    })
  ];
}
