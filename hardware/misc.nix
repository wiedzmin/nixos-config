{ config, lib, pkgs, ... }:

{
    boot.extraModulePackages = with config.boot.kernelPackages; [ exfat-nofuse ];

    services.upower.enable = true;

    environment.systemPackages = with pkgs; [
        # ocz-ssd-guru # add as an overlay and fix hash (and installation instructions)
        intelmetool
        me_cleaner
        smartmontools
        config.boot.kernelPackages.perf
    ];

    boot.kernel.sysctl."fs.inotify.max_user_instances" = 512;
    boot.kernel.sysctl."fs.inotify.max_user_watches" = 1048576;
}
