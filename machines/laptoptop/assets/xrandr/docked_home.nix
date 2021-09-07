{ config, inputs, lib, pkgs, ... }:
with import ../../../../modules/wmutil.nix { inherit config inputs lib pkgs; };
with import ../../../../modules/util.nix { inherit config inputs lib pkgs; };

let
  profileName = "docked-home-x270-temp";
  user = config.attributes.mainUser.name;
in
{
  nixpkgs.config.packageOverrides = _: rec {
    "rescreen-${profileName}-i3" = mkShellScriptWithDeps "rescreen-${profileName}-i3" (with pkgs; [ i3 ]) ''
      i3-msg --quiet "${
        mvWorkspacesCmdI3 config.wmCommon.workspaces "primary" config.attributes.hardware.monitors.externalPrimaryHead.name}${
        mvWorkspacesCmdI3 config.wmCommon.workspaces "secondary" config.attributes.hardware.monitors.internalHead.name
      }${mvWorkspacesCmdI3 config.wmCommon.workspaces "tertiary" config.attributes.hardware.monitors.internalHead.name}"
    '';
  };
  home-manager.users.${user} = {
    home.packages = [ pkgs."rescreen-${profileName}-i3" ];
    programs.autorandr = {
      profiles = {
        "${profileName}" = {
          fingerprint = {
            "${config.attributes.hardware.monitors.externalPrimaryHead.name}" =
              "00ffffffffffff001e6dbb594f530100061701036c3c2278ea3135a5554ea1260c5054a54b00714f81809500b300a9c0810081c09040023a801871382d40582c450056512100001e000000fd00384b1e530f000a202020202020000000fc003237454133330a202020202020000000ff0033303652414e4e324a3836330a00f2";
            "${config.attributes.hardware.monitors.internalHead.name}" =
              config.attributes.hardware.monitors.internalHead.edid;
          };
          config = {
            "${config.attributes.hardware.monitors.externalPrimaryHead.name}" = {
              enable = true;
              position = "0x0";
              mode = "1920x1080";
              gamma = config.workstation.randr.defaults.gamma;
              rate = config.workstation.randr.defaults.rate;
            } // lib.optionalAttrs (config.workstation.randr.heads.orientation.primary != "normal") {
              rotate = config.workstation.randr.heads.orientation.primary;
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
