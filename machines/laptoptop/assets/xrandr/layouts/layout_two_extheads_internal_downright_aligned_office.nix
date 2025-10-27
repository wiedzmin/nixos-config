{ config, lib, ... }:

{
  config.attributes.hardware.monitors.layouts = {
    twoExtHeadsInternalDownrightAlignedOffice = {
      "${config.attributes.hardware.monitors.externalPrimaryHead.name}" = {
        enable = true;
        primary = true;
        position = "0x0";
        mode = "1920x1200";
        gamma = config.workstation.randr.defaults.gamma;
        rate = "59.95";
      } // lib.optionalAttrs (config.workstation.randr.heads.orientation.primary != "normal") {
        rotate = config.workstation.randr.heads.orientation.primary;
      };
      "${config.attributes.hardware.monitors.externalSecondaryHead.name}" = {
        enable = true;
        position = "1920x0";
        mode = "1280x1024";
        gamma = config.workstation.randr.defaults.gamma;
        rate = "60.02";
      } // lib.optionalAttrs (config.workstation.randr.heads.orientation.primary != "normal") {
        rotate = config.workstation.randr.heads.orientation.primary;
      };
      "${config.attributes.hardware.monitors.internalHead.name}" = {
        enable = true;
        position = "0x1200";
        mode = config.attributes.hardware.monitors.internalHead.resolution;
        gamma = config.workstation.randr.defaults.gamma;
        rate = config.workstation.randr.defaults.rate;
      };
    };
  };
}
