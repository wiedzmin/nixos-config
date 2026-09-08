{ config, lib, ... }:
with lib;

# orientations: [ "normal" "left" "right" "inverted" ]

{
  options = {
    heads.layouts = {
      roaming = mkOption {
        type = types.attrs;
        default = {
          "${config.hardware.heads.internal-x270-SL10M37887.output}" = {
            position = "0x0";
            orientation = "normal";
          };
        };
        description = "XrandR spatial layout";
      };
    };
  };
}
