{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.workstation.video.transparency;
  prefix = config.wmCommon.prefix;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    workstation.video.transparency = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable transparency customizations.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM bindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      users.users.${user}.extraGroups = [ "video" ];
      home-manager.users.${user} = {
        services = {
          picom = {
            enable = true;
            backend = "glx";
            vSync = true;
            package = pkgs.picom;
            extraOptions = ''
              glx-no-rebind-pixmap = true;
              glx-no-stencil = true;
              xrender-sync-fence = true;
            '';
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ prefix "F8" ];
          cmd = "${pkgs.picom}/bin/picom-trans -c +10";
          mode = "root";
        }
        {
          key = [ prefix "F9" ];
          cmd = "${pkgs.picom}/bin/picom-trans -c -10";
          mode = "root";
        }
      ];
    })
  ];
}
