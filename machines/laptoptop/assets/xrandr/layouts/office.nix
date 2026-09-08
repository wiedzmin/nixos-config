{ config, lib, ... }:
with lib;

# orientations: [ "normal" "left" "right" "inverted" ]

{
  options = {
    heads.layouts = {
      office = mkOption {
        type = types.attrs;
        default = {
          "${config.hardware.heads._27EAxx-DSUB.output}" = {
            position = "0x0";
            orientation = "normal";
          };
          "${config.hardware.heads.PA248Q-DVI.output}" = {
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
