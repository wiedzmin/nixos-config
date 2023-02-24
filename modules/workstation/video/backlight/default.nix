{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.video.backlight;
  user = config.attributes.mainUser.name;
in
{
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
      redshift.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Redshift service";
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
        assertion = cfg.redshift.latitude != "" && cfg.redshift.longitude != "";
        message = "backlight/redshift: no location provided.";
      }];

      users.users."${user}".extraGroups = [ "video" ];
      programs.light.enable = true;
      hardware.brillo.enable = true;

      home-manager.users."${user}" = {
        services = {
          redshift = {
            inherit (cfg.redshift) enable latitude longitude;
            temperature.day = cfg.redshift.temperature.day;
            temperature.night = cfg.redshift.temperature.night;
            settings = {
              general = {
                adjustment-method = "randr";
                # gamma = 0.8;
              };
              # randr = { screen = 0; };
              redshift = {
                brightness-day = cfg.redshift.brightness.day;
                brightness-night = cfg.redshift.brightness.night;
              };
            };
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.common = [
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
