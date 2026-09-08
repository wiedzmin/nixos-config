{ config, lib, ... }:
with lib;

# orientations: [ "normal" "left" "right" "inverted" ]

{
  options = {
    heads.layouts = {
      home = mkOption {
        type = types.attrs;
        default = {
          "${config.hardware.heads._27U411A-B-DSUB.output}" = {
            position = "0x0";
            orientation = "normal";
          };
          "${config.hardware.heads._27U411A-B-HDMI.output}" = {
            position = "1366x1080";
            orientation = "normal";
          };
          "${config.hardware.heads.internal-x270-SL10M37887.output}" = {
            position = "0x1080";
            orientation = "normal";
          };
        };
        description = "XrandR spatial layout";
      };
    };
  };
}
