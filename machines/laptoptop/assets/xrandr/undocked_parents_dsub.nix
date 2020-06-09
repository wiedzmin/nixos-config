{ config, pkgs, lib, ... }:
with import ../../../../modules/common/wm/wmutil.nix { inherit config lib pkgs; };

{
  home-manager.users."${config.attributes.mainUser.name}" = {
    programs.autorandr = {
      profiles = {
        "undocked-parents-dsub" = {
          fingerprint = {
            VGA-1 =
              "00ffffffffffff004c2d0e0139314a4d100f01036c261e782aee95a3544c99260f5054bfef808180714f010101010101010101010101302a009851002a4030701300782d1100001e000000fd00384b1e510e000a202020202020000000fc0053796e634d61737465720a2020000000ff00485348593430323338330a202000d2";
            "${config.attributes.hardware.monitors.internalHead.name}" =
              config.attributes.hardware.monitors.internalHead.edid;
          };
          config = {
            VGA-1 = {
              enable = true;
              position = "0x0";
              mode = "1280x1024";
              gamma = config.custom.video.gamma;
              rate = config.custom.video.rate;
            };
            "${config.attributes.hardware.monitors.internalHead.name}" = {
              enable = true;
              primary = true;
              position = "0x1024";
              mode = config.attributes.hardware.monitors.internalHead.resolution;
              gamma = config.custom.video.gamma;
              rate = config.custom.video.rate;
            };
          };
        };
      };
    };
  };
}
