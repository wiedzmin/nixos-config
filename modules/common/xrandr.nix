{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.xrandr;
  custom = import ../../pkgs/custom pkgs config;
in {
  options = {
    xrandr = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable xrandr helper harness.
        '';
      };
      rate = mkOption {
        type = types.str;
        default = "60.00";
        description = ''
          Refresh rate for XRandR heads.
        '';
      };
      gamma = mkOption {
        type = types.str;
        default = "1.0:0.909:0.833";
        description = ''
          XRandR gamma settings.
        '';
      };
      autorandr.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable autorandr.
        '';
      };
      autorandr.hooks = mkOption {
        type = types.attrs;
        default = {};
        description = ''
          Autorandr hooks.
        '';
      };
      autorandr.profiles = mkOption {
        type = types.attrs;
        description = "Autorandr profiles.";
        default = { # TODO: think if there should be a default value
          "mobile" = {
            fingerprint = {
              "${config.attributes.hardware.monitors.internalHead.name}" =
                "${config.attributes.hardware.monitors.internalHead.edid}";
            };
            config = {
              "${config.attributes.hardware.monitors.internalHead.name}" = {
                enable = true;
                primary = true;
                position = "0x0";
                mode = "${config.attributes.hardware.monitors.internalHead.resolution}";
                gamma = "${cfg.gamma}";
                rate = "${cfg.rate}";
              };
            };
          };
          "docked-home" = {
            fingerprint = {
              "HDMI-2" =
                "00ffffffffffff001e6dbc594f53010006170103803c2278ea3135a5554ea1260c5054a54b00714f81809500b300a9c0810081c09040023a801871382d40582c450056512100001e000000fd00384b1e530f000a202020202020000000fc003237454133330a202020202020000000ff0033303652414e4e324a3836330a00dd";
              "HDMI-3" =
                "00ffffffffffff000469b124010101011d18010380372378ea3d15a3544da027125054bfef00714f818081409500a940b300d1c00101283c80a070b023403020360022602100001a000000fd00324c1e5311000a202020202020000000fc0050413234380a20202020202020000000ff0045374c4d51533037373132380a0023";
              "${config.attributes.hardware.monitors.internalHead.name}" =
                "${config.attributes.hardware.monitors.internalHead.edid}";
            };
            config = {
              "HDMI-2" = {
                enable = true;
                position = "0x0";
                mode = "1920x1080";
                gamma = "${cfg.gamma}";
                rate = "${cfg.rate}";
              };
              "HDMI-3" = {
                enable = true;
                position = "1366x1080";
                mode = "1920x1080";
                gamma = "${cfg.gamma}";
                rate = "${cfg.rate}";
                rotate = "left";
              };
              "${config.attributes.hardware.monitors.internalHead.name}" = {
                enable = true;
                primary = true;
                position = "0x1080";
                mode = "${config.attributes.hardware.monitors.internalHead.resolution}";
                gamma = "${cfg.gamma}";
                rate = "${cfg.rate}";
              };
            };
          };
          "undocked-parents-dsub" = {
            fingerprint = {
              "VGA-1" =
                "00ffffffffffff004c2d0e0139314a4d100f01036c261e782aee95a3544c99260f5054bfef808180714f010101010101010101010101302a009851002a4030701300782d1100001e000000fd00384b1e510e000a202020202020000000fc0053796e634d61737465720a2020000000ff00485348593430323338330a202000d2";
              "${config.attributes.hardware.monitors.internalHead.name}" =
                "${config.attributes.hardware.monitors.internalHead.edid}";
            };
            config = {
              "VGA-1" = {
                enable = true;
                position = "0x0";
                mode = "1280x1024";
                gamma = "${cfg.gamma}";
                rate = "${cfg.rate}";
              };
              "${config.attributes.hardware.monitors.internalHead.name}" = {
                enable = true;
                primary = true;
                position = "0x1024";
                mode = "${config.attributes.hardware.monitors.internalHead.resolution}";
                gamma = "${cfg.gamma}";
                rate = "${cfg.rate}";
              };
            };
          };
        };
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.autorandr.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.autorandr = {
          enable = true;
          hooks = lib.optionalAttrs
            (config.home-manager.users."${config.attributes.mainUser.name}".services.compton.enable) {
              predetect = { "kill-compton" = "${custom.kill-compton}/bin/kill-compton"; };
          } // cfg.autorandr.hooks;
          profiles = cfg.autorandr.profiles;
        };
      };
    })
  ];
}
