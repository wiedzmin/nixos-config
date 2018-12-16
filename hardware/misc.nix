{ config, lib, pkgs, ... }:

{
    boot.extraModulePackages = with config.boot.kernelPackages; [ exfat-nofuse ];

    services.upower.enable = true;

    environment.systemPackages = with pkgs; [
        smartmontools
    ];
}
