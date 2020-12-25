{ config, inputs, lib, pkgs, ... }:
with import ../../../../modules/wmutil.nix { inherit config inputs lib pkgs; };
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
    set-all-tabbed-ws-i3 = mkShellScriptWithDeps "set-all-tabbed-ws-i3" (with pkgs; [ i3 ]) ''
      i3-msg --quiet "${
        setWorkspacesLayoutI3 config.wmCommon.workspaces "primary" "tabbed"
      }${
        setWorkspacesLayoutI3 config.wmCommon.workspaces "secondary" "tabbed"
      }${setWorkspacesLayoutI3 config.wmCommon.workspaces "tertiary" "tabbed"}"
    '';
  };
  home-manager.users.${user} = {
    home.packages = [ pkgs."rescreen-${profileName}-i3" pkgs.set-all-tabbed-ws-i3 ];
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
          hooks.postswitch = ''
            rescreen-${profileName}-i3
            set-all-tabbed-ws-i3
            ${pkgs.i3}/bin/i3-msg --quiet "workspace next_on_output"
          '';
          # TODO: activate some non-empty workspace afterwards
        };
      };
    };
  };
}
