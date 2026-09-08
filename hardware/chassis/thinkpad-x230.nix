{ inputs, ... }:

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
  };
  attributes.fonts.monospaced.fillColumn = 190;
}
