{ config, lib, pkgs, ... }:

{
    boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];

    services.upower.enable = true;
}
