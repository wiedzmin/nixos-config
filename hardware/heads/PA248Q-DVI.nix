{ config, lib, ... }:
with lib;

{
  options = {
    hardware.heads.PA248Q-DVI = {
      EDID = mkOption {
        type = types.str;
        default =
          "00ffffffffffff000469b12401010101" +
          "0f17010380372378ea3d15a3544da027" +
          "125054bfef00714f818081409500a940" +
          "b300d1c00101283c80a070b023403020" +
          "360022602100001a000000fd00324c1e" +
          "5311000a202020202020000000fc0050" +
          "413234380a20202020202020000000ff" +
          "0044344c4d51533034313530370a003e";
        description = "EDID value";
      };
      output = mkOption {
        type = types.str;
        default = "DP-2-2";
        description = "Output name";
      };
      mode = mkOption {
        type = types.str;
        default = "1920x1200";
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
