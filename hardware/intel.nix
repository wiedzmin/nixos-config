{ config, lib, pkgs, ... }:

{
    # TODO: think of extracting kernel-related options
    boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sdhci_pci" ];
    # boot.kernelModules = [ "kvm-intel" ];
    hardware.cpu.intel.updateMicrocode = true;
    hardware.enableAllFirmware = true;
    # boot.kernelPackages = pkgs.linuxPackages_latest;
    boot.extraModprobeConfig = ''
    #options iwlwifi 11n_disable=1 power_save=0
    '';
    nix.maxJobs = lib.mkDefault 4;
    nix.buildCores = lib.mkDefault 4;

    system.activationScripts.ensureBacklightPermissions = ''
        chmod a+w /sys/class/backlight/intel_backlight/brightness
    '';
}
