{ config, lib, pkgs, ... }:
with import ../../../pkgs/util.nix { inherit config pkgs lib; };
with lib;

let cfg = config.polkit-silent-auth;
in {
  options = {
    polkit-silent-auth = {
      enable = mkOption { # TODO: change to mkEnableOption
        type = types.bool;
        default = false;
        description = ''
          Whether to allow special users to do some things  without authentication.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    security.polkit.extraConfig = ''
      /* Allow users in wheel group to manage systemd units without authentication */
      polkit.addRule(function(action, subject) {
          if (action.id == "org.freedesktop.systemd1.manage-units" &&
              subject.isInGroup("wheel")) {
              return polkit.Result.YES;
          }
      });

      /* Allow users in wheel group to run programs with pkexec without authentication */
      polkit.addRule(function(action, subject) {
          if (action.id == "org.freedesktop.policykit.exec" &&
              subject.isInGroup("wheel")) {
              return polkit.Result.YES;
          }
      });
    '';
  };
}
