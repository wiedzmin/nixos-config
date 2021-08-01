{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.workstation.performance;
  user = config.attributes.mainUser.name;
in
{
  options = {
    workstation.performance = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable performance customizations.";
      };
      appsSuspension.rules = mkOption {
        type = types.attrs;
        default = { };
        description = "Apps suspending rules.";
      };
      warmup.paths = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Paths to pull.";
      };
      oom.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable userspace oom solution(s)";
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
      boot = {
        kernelParams = [
          "l1tf=off"
          "nospec_store_bypass_disable"
          "nospectre_v1"
          "nospectre_v2"
          "pti=off"
          "scsi_mod.use_blk_mq=1"
        ];
        kernelModules = [ "bfq" ];
      };

      services = {
        udev.extraRules = ''
          ACTION=="add|change", KERNEL=="sd[ab][!0-9]", ATTR{queue/scheduler}="kyber"
        '';
        irqbalance.enable = true;
        earlyoom.enable = cfg.oom.enable;
      };

      home-manager.users.${user} = {
        services.xsuspender = optionalAttrs (cfg.appsSuspension.rules != { })
          {
            enable = true;
            defaults = {
              suspendDelay = 10;
              onlyOnBattery = false;
            };
            rules = cfg.appsSuspension.rules;
          };
      };
      systemd.user.services = optionalAttrs (cfg.warmup.paths != [ ]) {
        "warmup" = {
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
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable && cfg.appsSuspension.rules != { }) {
      wmCommon.keys = [
        {
          key = [ "x" ];
          cmd = "${pkgs.systemd}/bin/systemctl --user restart xsuspender.service";
          mode = "services";
        }
        {
          key = [ "Shift" "x" ];
          cmd = "${pkgs.systemd}/bin/systemctl --user stop xsuspender.service";
          mode = "services";
        }
      ];
    })
  ];
}
