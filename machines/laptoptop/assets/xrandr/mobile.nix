{ config, inputs, lib, pkgs, ... }:
with import ../../../../modules/common/wm/wmutil.nix { inherit config inputs lib pkgs; };
with import ../../../../modules/util.nix { inherit config inputs lib pkgs; };

let
  profileName = "mobile";
  user = config.attributes.mainUser.name;
in {
  nixpkgs.config.packageOverrides = _: rec {
    "rescreen-${profileName}-i3" = mkShellScriptWithDeps "rescreen-${profileName}-i3" (with pkgs; [ i3 ]) ''
      i3-msg --quiet "${
        mvWorkspacesI3Cmd config.wmCommon.workspaces "primary" config.attributes.hardware.monitors.internalHead.name
      }${
        mvWorkspacesI3Cmd config.wmCommon.workspaces "secondary" config.attributes.hardware.monitors.internalHead.name
      }${mvWorkspacesI3Cmd config.wmCommon.workspaces "tertiary" config.attributes.hardware.monitors.internalHead.name}"
    '';
  };
  home-manager.users.${user} = {
    home.packages = [ pkgs."rescreen-${profileName}-i3" ];
    programs.autorandr = {
      profiles = {
        "${profileName}" = {
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
          hooks.postswitch = "rescreen-${profileName}-i3";
          # TODO: activate some non-empty workspace afterwards
        };
      };
    };
  };
}
