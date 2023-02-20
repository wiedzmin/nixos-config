{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.video.opengl;
in
{
  options = {
    workstation.video.opengl = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable OpenGL";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      hardware.opengl = {
        enable = true;
        extraPackages = with pkgs; [ intel-media-driver libvdpau-va-gl vaapiIntel vaapiVdpau ];
        driSupport32Bit = true;
        extraPackages32 = with pkgs.pkgsi686Linux; [ libvdpau-va-gl vaapiIntel vaapiVdpau ];
      };
      shell.core.variables = [{ LIBVA_DRIVER_NAME = "iHD"; global = true; }];
    })
  ];
}
