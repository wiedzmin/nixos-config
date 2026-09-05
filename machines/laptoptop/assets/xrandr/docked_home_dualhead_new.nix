{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  profileName = "docked-home-x270-dualhead-new";
  user = config.attributes.mainUser.name;
in
{
  nixpkgs.config.packageOverrides = _: {
    "rescreen-${profileName}-i3" = pkgs.writeShellApplication {
      name = "rescreen-${profileName}-i3";
      runtimeInputs = with pkgs; [ i3 ];
      text = ''
        i3-msg --quiet "${
          mvWorkspacesCmdI3 config.wmCommon.workspaces "primary" config.attributes.hardware.monitors.externalPrimaryHead.name}${
          mvWorkspacesCmdI3 config.wmCommon.workspaces "secondary" config.attributes.hardware.monitors.externalSecondaryHead.name
        }${mvWorkspacesCmdI3 config.wmCommon.workspaces "tertiary" config.attributes.hardware.monitors.internalHead.name}"
      '';
    };
  };
  home-manager.users."${user}" = {
    home.packages = [ pkgs."rescreen-${profileName}-i3" ];
    programs.autorandr = {
      profiles = {
        "${profileName}" = {
          fingerprint = {
            "${config.attributes.hardware.monitors.externalPrimaryHead.name}" =
              "00ffffffffffff001e6d675cc678000003240103083c22782a6015ab514b9d24105054a54b00714f8140818081c081009500b3000101023a801871382d40582c450056502100001e2a4480a0703827403020350056502100001a000000fd00304b1e5612000a202020202020000000fc004c47204648440a20202020202000ea";
            "${config.attributes.hardware.monitors.externalSecondaryHead.name}" =
              "00ffffffffffff000469b124010101011d18010380372378ea3d15a3544da027125054bfef00714f818081409500a940b300d1c00101283c80a070b023403020360022602100001a000000fd00324c1e5311000a202020202020000000fc0050413234380a20202020202020000000ff0045374c4d51533037373132380a0023";
            "${config.attributes.hardware.monitors.internalHead.name}" =
              config.attributes.hardware.monitors.internalHead.edid;
          };
          config = config.attributes.hardware.monitors.layouts.twoExtHeadsInternalAtRight;
          hooks.postswitch = lib.optionalString (config.wm.i3.enable) "rescreen-${profileName}-i3";
        };
      };
    };
  };
}
