{ config, lib, ... }:
with lib;

{
  options = {
    hardware.heads.internal-x270-SL10M37887 = {
      # Identification (P/N) number of the laptop frame
      EDID = mkOption {
        type = types.str;
        default =
          "00ffffffffffff0026cfe50400000000" +
          "00180104951c10780a12309156539228" +
          "1e505400000001010101010101010101" +
          "010101010101201c5686500020300808" +
          "8800149b100000198016568650002030" +
          "08088800149b10000019000000fe0049" +
          "6e666f566973696f6e0a2020000000fe" +
          "004d3132354e575233205230200a00f1";
        description = "EDID value";
      };
      output = mkOption {
        type = types.str;
        default = "eDP-1";
        description = "Output name";
      };
      mode.hardware = mkOption {
        type = types.str;
        default = "1366x768";
        description = "Hardware resolution mode";
      };
      mode.Xephyr = mkOption {
        type = types.str;
        default = "1200x600";
        description = "Resolution mode for Xephyr X server";
      };
      gamma = mkOption {
        type = types.str;
        default = config.workstation.randr.defaults.gamma;
        description = "Gamma value";
      };
      rate = mkOption {
        type = types.str;
        default = config.workstation.randr.defaults.rate;
        description = "Refresh rate";
      };
    };
  };
}
