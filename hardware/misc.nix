{ config, lib, pkgs, ... }:

{
    boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];
    boot.kernelPatches = [
        {
            name = "edid-config";
            patch = null;
            extraConfig = ''
                DRM_LOAD_EDID_FIRMWARE y
          '';
        }
    ];
}
