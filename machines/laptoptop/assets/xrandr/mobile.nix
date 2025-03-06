{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  profileName = "mobile";
  user = config.attributes.mainUser.name;
in
{
  nixpkgs.config.packageOverrides = _: {
    "rescreen-${profileName}-i3" = pkgs.writeShellApplication {
      name = "rescreen-${profileName}-i3";
      runtimeInputs = with pkgs; [ i3 ];
      text = ''
        i3-msg --quiet "${
          mvWorkspacesCmdI3 config.wmCommon.workspaces "primary" config.attributes.hardware.monitors.internalHead.name
        }${
          mvWorkspacesCmdI3 config.wmCommon.workspaces "secondary" config.attributes.hardware.monitors.internalHead.name
        }${mvWorkspacesCmdI3 config.wmCommon.workspaces "tertiary" config.attributes.hardware.monitors.internalHead.name}"
        ${pkgs.xorg.xdpyinfo}/bin/xdpyinfo | grep dimensions | cut -f2 -d: | tr -s ' ' | cut -d ' ' -f2 | tr -d '\n' | redis-cli -x set wm/dimensions
      '';
    };
  };
  home-manager.users."${user}" = {
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
              gamma = config.workstation.randr.defaults.gamma;
              rate = config.workstation.randr.defaults.rate;
            };
          };
          hooks.postswitch = lib.optionalString (config.wm.i3.enable) ''
            rescreen-${profileName}-i3
            ${pkgs.i3}/bin/i3-msg --quiet "workspace back_and_forth"
          '';
        };
      };
    };
  };
}
