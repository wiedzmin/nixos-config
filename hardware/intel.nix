{ config, lib, pkgs, ... }:

{
    boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sdhci_pci" ];
    # boot.kernelModules = [ "kvm-intel" ];
    hardware.cpu.intel.updateMicrocode = true;
    hardware.enableAllFirmware = true;
    boot.extraModprobeConfig = ''
    #options iwlwifi 11n_disable=1 power_save=0
    '';
    nix.maxJobs = lib.mkDefault 4;
}
