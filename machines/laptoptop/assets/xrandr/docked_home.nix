{ config, inputs, lib, pkgs, ... }:
with import ../../../../modules/wmutil.nix { inherit config inputs lib pkgs; };
with import ../../../../modules/util.nix { inherit config inputs lib pkgs; };

let
  profileName = "docked-home";
  user = config.attributes.mainUser.name;
in {
  nixpkgs.config.packageOverrides = _: rec {
    "rescreen-${profileName}-i3" = mkShellScriptWithDeps "rescreen-${profileName}-i3" (with pkgs; [ i3 ]) ''
      i3-msg --quiet "${mvWorkspacesCmdI3 config.wmCommon.workspaces "primary" "HDMI-2"}${
        mvWorkspacesCmdI3 config.wmCommon.workspaces "secondary" "HDMI-3"
      }${mvWorkspacesCmdI3 config.wmCommon.workspaces "tertiary" config.attributes.hardware.monitors.internalHead.name}"
    '';
  };
  home-manager.users.${user} = {
    home.packages = [ pkgs."rescreen-${profileName}-i3" ];
    programs.autorandr = {
      profiles = {
        "${profileName}" = {
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
              gamma = config.workstation.randr.defaults.gamma;
              rate = config.workstation.randr.defaults.rate;
            } // lib.optionalAttrs (config.workstation.randr.heads.orientation.primary != "normal") {
              rotate = config.workstation.randr.heads.orientation.primary;
            };
            HDMI-3 = {
              enable = true;
              position = "1366x1080";
              mode = "1920x1080";
              gamma = config.workstation.randr.defaults.gamma;
              rate = config.workstation.randr.defaults.rate;
            } // lib.optionalAttrs (config.workstation.randr.heads.orientation.secondary != "normal") {
              rotate = config.workstation.randr.heads.orientation.secondary;
            };
            "${config.attributes.hardware.monitors.internalHead.name}" = {
              enable = true;
              primary = true;
              position = "0x1080";
              mode = config.attributes.hardware.monitors.internalHead.resolution;
              gamma = config.workstation.randr.defaults.gamma;
              rate = config.workstation.randr.defaults.rate;
            };
          };
          hooks.postswitch = lib.optionalString (config.wm.i3.enable) "rescreen-${profileName}-i3";
        };
      };
    };
  };
}
