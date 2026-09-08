{ config, lib, ... }:
with lib;

{
  options = {
    hardware.heads._27U411A-B-HDMI = {
      EDID = mkOption {
        type = types.str;
        default =
          "00ffffffffffff001e6dd95c36780000" +
          "03240103803c2278ea6015ab514b9d24" +
          "105054a54b00714f8140818081c0b300" +
          "810095000101023a801871382d40582c" +
          "450056502100001e000000ff00363033" +
          "544f554830573737340a000000fc004c" +
          "47204648440a202020202020000000fd" +
          "0030781e8c22000a2020202020200154" +
          "020332f123090707489001030412131f" +
          "3f67030c00100038446ad85dc4014b80" +
          "00003078e305c301e200cae606050152" +
          "52482a4480a070382740302035005650" +
          "2100001a605980a07038144030203500" +
          "56502100001a396c80a070381e403020" +
          "350056502100001a0000000000000000" +
          "000000000000000000000000000000bd";
        description = "EDID value";
      };
      output = mkOption {
        type = types.str;
        default = "DP-2-1";
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
