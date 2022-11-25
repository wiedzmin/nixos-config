{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.video.transparency;
  user = config.attributes.mainUser.name;
  inherit (config.wmCommon) prefix;
in
{
  options = {
    workstation.video.transparency = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable transparency customizations.";
      };
      opacityRules = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "View <home-manager.services.picom.description>";
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
      users.users."${user}".extraGroups = [ "video" ];
      home-manager.users."${user}" = {
        services = {
          picom = {
            enable = true;
            backend = "glx";
            vSync = true;
            package = pkgs.picom;
            inherit (cfg) opacityRules;
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.common = [
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
