{ config, lib, ... }:

{
  config.attributes.hardware.monitors.layouts = {
    twoExtHeadsInternalDownrightAligned = {
      "${config.attributes.hardware.monitors.externalPrimaryHead.name}" = {
        enable = true;
        position = "0x0";
        mode = "1920x1200";
        gamma = config.workstation.randr.defaults.gamma;
        rate = config.workstation.randr.defaults.rate;
      } // lib.optionalAttrs (config.workstation.randr.heads.orientation.primary != "normal") {
        rotate = config.workstation.randr.heads.orientation.primary;
      };
      "${config.attributes.hardware.monitors.externalSecondaryHead.name}" = {
        enable = true;
        position = "1366x1200";
        mode = "1920x1080";
        gamma = config.workstation.randr.defaults.gamma;
        rate = config.workstation.randr.defaults.rate;
      } // lib.optionalAttrs (config.workstation.randr.heads.orientation.secondary != "normal") {
        rotate = config.workstation.randr.heads.orientation.secondary;
      };
      "${config.attributes.hardware.monitors.internalHead.name}" = {
        enable = true;
        primary = true;
        position = "0x1200";
        mode = config.attributes.hardware.monitors.internalHead.resolution;
        gamma = config.workstation.randr.defaults.gamma;
        rate = config.workstation.randr.defaults.rate;
      };
    };
  };
}
