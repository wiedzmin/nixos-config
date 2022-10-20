{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  imports = [
    "${inputs.nixos-hardware}/common/cpu/intel/kaby-lake"
    "${inputs.nixos-hardware}/lenovo/thinkpad/x270"
  ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
    wirelessRegulatoryDatabase = true;
    ksm.enable = true;
    sensor.iio.enable = true;
  };

  boot = {
    kernelModules = [ "thinkpad_acpi" "thinkpad_hwmon" ];
  };

  attributes.hardware = {
    dmiSystemVersion = "ThinkPad X270";
    monitors = {
      internalHead.name = "eDP-1";
      internalHead.resolution = "1366x768";
      internalHead.resolutionXephyr = "1200x600";
    };
  };

  attributes.hardware.inputDevices = {
    keyboard = [
      "/dev/input/by-path/platform-i8042-serio-0-event-kbd"
      "/dev/input/event19"
      "/dev/input/event3"
    ];
    mouse = [
      "/dev/input/by-path/platform-i8042-serio-0-event-mouse"
      "/dev/input/event16"
      "/dev/input/event18"
    ];
  };
}
