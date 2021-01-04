{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.workstation.video.backlight;
  prefix = config.wmCommon.prefix;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    workstation.video.backlight = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable backlight controls.";
      };
      delta = mkOption {
        type = types.int;
        default = 10;
        description = "Backlight delta percents.";
      };
      redshift.latitude = mkOption {
        type = types.str;
        default = "";
        description = "Latitude for Redshift service";
      };
      redshift.longitude = mkOption {
        type = types.str;
        default = "";
        description = "Longitude for Redshift service";
      };
      redshift.temperature.day = mkOption {
        type = types.int;
        default = 5500;
        description = "Day color temperature for Redshift service";
      };
      redshift.temperature.night = mkOption {
        type = types.int;
        default = 3100;
        description = "Night color temperature for Redshift service";
      };
      redshift.brightness.day = mkOption {
        type = types.str;
        default = "1.0";
        description = "Day screen brightness for Redshift service";
      };
      redshift.brightness.night = mkOption {
        type = types.str;
        default = "0.7";
        description = "Night screen brightness for Redshift service";
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
      assertions = [{
        assertion = (cfg.redshift.latitude != "" && cfg.redshift.longitude != "");
        message = "backlight/redshift: no location provided.";
      }];

      users.users.${user}.extraGroups = [ "video" ];
      programs.light.enable = true;
      hardware.brillo.enable = true;
      home-manager.users.${user} = {
        services = {
          redshift = {
            enable = true;
            latitude = cfg.redshift.latitude;
            longitude = cfg.redshift.longitude;
            temperature.day = cfg.redshift.temperature.day;
            temperature.night = cfg.redshift.temperature.night;
            brightness.day = cfg.redshift.brightness.day;
            brightness.night = cfg.redshift.brightness.night;
            extraOptions = [ "-v" "-m randr" ];
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "XF86MonBrightnessDown" ];
          cmd = "${pkgs.light}/bin/light -U ${toString cfg.delta}";
          mode = "root";
        }
        {
          key = [ "XF86MonBrightnessUp" ];
          cmd = "${pkgs.light}/bin/light -A ${toString cfg.delta}";
          mode = "root";
        }
        {
          key = [ "Control" "XF86MonBrightnessDown" ];
          cmd = "${pkgs.light}/bin/light -S 20";
          mode = "root";
        }
        {
          key = [ "Control" "XF86MonBrightnessUp" ];
          cmd = "${pkgs.light}/bin/light -S 100";
          mode = "root";
        }
        {
          key = [ "s" ];
          cmd = "${pkgs.systemd}/bin/systemctl --user restart redshift.service";
          mode = "xserver";
        }
        {
          key = [ "Control" "s" ];
          cmd = "${pkgs.systemd}/bin/systemctl --user stop redshift.service";
          mode = "xserver";
        }
      ];
    })
  ];
}
