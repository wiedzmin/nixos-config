{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  imports = [
    "${inputs.nixos-hardware}/common/cpu/intel/sandy-bridge"
    "${inputs.nixos-hardware}/lenovo/thinkpad/x230"
  ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
    ksm.enable = true;
    sensor.iio.enable = true;
  };

  boot = {
    kernelModules = [ "thinkpad_acpi" "thinkpad_hwmon" ];
  };

  attributes.hardware = {
    dmiSystemVersion = "ThinkPad X230";
    monitors = {
      internalHead.name = "LVDS-1";
      internalHead.resolution = "1366x768";
      internalHead.resolutionXephyr = "1200x600";
    };
  };
}
