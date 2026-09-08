{ config, lib, ... }:
with lib;

{
  options = {
    hardware.heads._27EAxx-DSUB = {
      EDID = mkOption {
        type = types.str;
        default =
          "00ffffffffffff001e6dbb594f530100" +
          "061701036c3c2278ea3135a5554ea126" +
          "0c5054a54b00714f81809500b300a9c0" +
          "810081c09040023a801871382d40582c" +
          "450056512100001e000000fd00384b1e" +
          "530f000a202020202020000000fc0032" +
          "37454133330a202020202020000000ff" +
          "0033303652414e4e324a3836330a00f2";
        description = "EDID value";
      };
      output = mkOption {
        type = types.str;
        default = "DP-2-3";
        description = "Output name";
      };
      mode = mkOption {
        type = types.str;
        default = "1920x1080";
        description = "Resolution mode";
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
