{ config, inputs, lib, pkgs, ... }:
with import ../../../../modules/util.nix { inherit config inputs lib pkgs; };
with import ../../../../modules/wm/i3/util.nix { inherit config inputs lib pkgs; };

let
  profileName = "mobile";
  user = config.attributes.mainUser.name;
in
{
  nixpkgs.config.packageOverrides = _: rec {
    "rescreen-${profileName}-i3" = pkgs.writeShellApplication {
      name = "rescreen-${profileName}-i3";
      runtimeInputs = with pkgs; [ i3 ];
      text = ''
        i3-msg --quiet "${
          mvWorkspacesCmdI3 config.wmCommon.workspaces "primary" config.attributes.hardware.monitors.internalHead.name
        }${
          mvWorkspacesCmdI3 config.wmCommon.workspaces "secondary" config.attributes.hardware.monitors.internalHead.name
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
