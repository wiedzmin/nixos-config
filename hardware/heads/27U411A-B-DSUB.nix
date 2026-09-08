{ config, lib, ... }:
with lib;

{
  options = {
    hardware.heads._27U411A-B-DSUB = {
      EDID = mkOption {
        type = types.str;
        default =
          "00ffffffffffff001e6d675cc6780000" +
          "03240103083c22782a6015ab514b9d24" +
          "105054a54b00714f8140818081c08100" +
          "9500b3000101023a801871382d40582c" +
          "450056502100001e2a4480a070382740" +
          "3020350056502100001a000000fd0030" +
          "4b1e5612000a202020202020000000fc" +
          "004c47204648440a20202020202000ea";
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
