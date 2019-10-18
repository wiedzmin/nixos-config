{ config, lib, pkgs, ... }:
with lib;

let cfg = config.tools.system;
in {
  options = {
    tools.system = {
      graphics.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable graphics-related tools.";
      };
      hardware.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable structured JSON manipulation tools.";
      };
      forensics.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable various "system forensics" tools.'';
      };
      monitoring.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable monitoring tools.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.graphics.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          drm_info
          xtruss
        ];
      };
    })
    (mkIf cfg.hardware.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          acpitool
          dmidecode
          iw
          lshw
          pciutils
          usbutils
          wirelesstools
        ];
      };
    })
    (mkIf cfg.forensics.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          extrace
          libwhich
          lsof
          ltrace
          strace
        ];
      };
    })
    (mkIf cfg.monitoring.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          gotop
          iotop
          nmon
          pagemon
          procs
        ];
      };
    })
  ];
}
