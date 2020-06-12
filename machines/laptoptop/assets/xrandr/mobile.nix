{ config, pkgs, lib, ... }:
with import ../../../../modules/common/wm/wmutil.nix { inherit config lib pkgs; };

{
  home-manager.users."${config.attributes.mainUser.name}" = {
    programs.autorandr = {
      profiles = {
        "mobile" = {
          fingerprint = {
            "${config.attributes.hardware.monitors.internalHead.name}" =
              config.attributes.hardware.monitors.internalHead.edid;
          };
          config = {
            "${config.attributes.hardware.monitors.internalHead.name}" = {
              enable = true;
              primary = true;
              position = "0x0";
              mode = config.attributes.hardware.monitors.internalHead.resolution;
              gamma = config.custom.video.gamma;
              rate = config.custom.video.rate;
            };
          };
          hooks.postswitch = ''
            i3-msg --quiet "${
              mvWorkspacesI3Cmd config.wmCommon.workspaces.primary config.attributes.hardware.monitors.internalHead.name
            }${
              mvWorkspacesI3Cmd config.wmCommon.workspaces.secondary
              config.attributes.hardware.monitors.internalHead.name
            }${
              mvWorkspacesI3Cmd config.wmCommon.workspaces.tertiary
              config.attributes.hardware.monitors.internalHead.name
            }"
                      '';
        };
      };
    };
  };
}
