{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.performance;
in
{
  options = {
    workstation.performance = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable performance customizations.";
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
          "hugepages=5"
          "hugepagesz=1GB"
          "l1tf=off"
          "mds=off"
          "no_stf_barrier"
          "noibpb"
          "noibrs"
          "nospec_store_bypass_disable"
          "nospectre_v1"
          "nospectre_v2"
          "pti=off"
          "scsi_mod.use_blk_mq=1"
          "tsx=on"
          "tsx_async_abort=off"
        ];
        kernelModules = [ "bfq" ];
      };

      services = {
        udev.extraRules = ''
          ACTION=="add|change", KERNEL=="sd[ab][!0-9]", ATTR{queue/scheduler}="kyber"
        '';
        irqbalance.enable = true;
        systembus-notify.enable = lib.mkForce true;
        earlyoom = {
          enable = cfg.oom.enable;
          freeMemThreshold = 10;
          enableNotifications = true;
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
  ];
}
