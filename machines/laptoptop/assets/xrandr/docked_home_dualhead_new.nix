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
              "00ffffffffffff001e6dd95c3678000003240103803c2278ea6015ab514b9d24105054a54b00714f8140818081c0b300810095000101023a801871382d40582c450056502100001e000000ff00363033544f554830573737340a000000fc004c47204648440a202020202020000000fd0030781e8c22000a2020202020200154020332f123090707489001030412131f3f67030c00100038446ad85dc4014b8000003078e305c301e200cae60605015252482a4480a0703827403020350056502100001a605980a0703814403020350056502100001a396c80a070381e403020350056502100001a0000000000000000000000000000000000000000000000bd";
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
