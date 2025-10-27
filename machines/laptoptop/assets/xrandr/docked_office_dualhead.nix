{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  profileName = "docked-office-dualhead";
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
              "00ffffffffffff000469b124010101010f1701031e372378ea3d15a3544da027125054bfef00714f818081409500a940b300d1c00101283c80a070b023403020360022602100001a000000fd00324c1e5311000a202020202020000000fc0050413234380a20202020202020000000ff0044344c4d51533034313530370a00a0";
            "${config.attributes.hardware.monitors.externalSecondaryHead.name}" =
              "00ffffffffffff004c2d0f0139314a4d100f010380261e782aee95a3544c99260f5054bfef808180714f010101010101010101010101302a009851002a4030701300782d1100001e000000fd00384b1e510e000a202020202020000000fc0053796e634d61737465720a2020000000ff00485348593430323338330a202000bd";
            "${config.attributes.hardware.monitors.internalHead.name}" =
              config.attributes.hardware.monitors.internalHead.edid;
          };
          config = config.attributes.hardware.monitors.layouts.twoExtHeadsInternalDownrightAlignedOffice;
          hooks.postswitch = lib.optionalString (config.wm.i3.enable) "rescreen-${profileName}-i3";
        };
      };
    };
  };
}
