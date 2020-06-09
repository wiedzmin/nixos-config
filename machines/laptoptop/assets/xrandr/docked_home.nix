{ config, pkgs, lib, ... }:
with import ../../../../modules/common/wm/wmutil.nix { inherit config lib pkgs; };

{
  home-manager.users."${config.attributes.mainUser.name}" = {
    programs.autorandr = {
      profiles = {
        "docked-home" = {
          fingerprint = {
            HDMI-2 =
              "00ffffffffffff001e6dbc594f53010006170103803c2278ea3135a5554ea1260c5054a54b00714f81809500b300a9c0810081c09040023a801871382d40582c450056512100001e000000fd00384b1e530f000a202020202020000000fc003237454133330a202020202020000000ff0033303652414e4e324a3836330a00dd";
            HDMI-3 =
              "00ffffffffffff000469b124010101011d18010380372378ea3d15a3544da027125054bfef00714f818081409500a940b300d1c00101283c80a070b023403020360022602100001a000000fd00324c1e5311000a202020202020000000fc0050413234380a20202020202020000000ff0045374c4d51533037373132380a0023";
            "${config.attributes.hardware.monitors.internalHead.name}" =
              config.attributes.hardware.monitors.internalHead.edid;
          };
          config = {
            HDMI-2 = {
              enable = true;
              position = "0x0";
              mode = "1920x1080";
              gamma = config.custom.video.gamma;
              rate = config.custom.video.rate;
            };
            HDMI-3 = {
              enable = true;
              position = "1366x1080";
              mode = "1920x1080";
              gamma = config.custom.video.gamma;
              rate = config.custom.video.rate;
            } // lib.optionalAttrs (config.custom.video.rotateSecondaryHead) { rotate = config.custom.video.rotation; };
            "${config.attributes.hardware.monitors.internalHead.name}" = {
              enable = true;
              primary = true;
              position = "0x1080";
              mode = config.attributes.hardware.monitors.internalHead.resolution;
              gamma = config.custom.video.gamma;
              rate = config.custom.video.rate;
            };
          };
          hooks.postswitch = ''
            i3-msg --quiet "${mvWorkspacesI3Msg config.wmCommon.workspaces.primary "HDMI-2"}${
              mvWorkspacesI3Msg config.wmCommon.workspaces.secondary "HDMI-3"
            }${
              mvWorkspacesI3Msg config.wmCommon.workspaces.tertiary
              config.attributes.hardware.monitors.internalHead.name
            }"
                      '';
        };
      };
    };
  };
}
