{ config, inputs, lib, pkgs, ... }:
with import ../../modules/util.nix { inherit config inputs lib pkgs; };

{
  imports = [
    "${inputs.nixos-hardware}/common/cpu/intel/kaby-lake"
    "${inputs.nixos-hardware}/lenovo/thinkpad/x270"
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

  attributes.hardware.dmiSystemVersion = "ThinkPad X270";
}
