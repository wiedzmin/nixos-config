{ config, pkgs, lib, ... }:

{
  imports = [
    ../pkgs/setup.nix
    ../modules
    ../users/kotya/services/xserver.nix
    ../users/kotya/default.nix
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos-root";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/nixos-boot";
    fsType = "ext2";
  };

  swapDevices = [ ];

  nix = {
    # per-machine settings
    maxJobs = lib.mkDefault 4;
    buildCores = lib.mkDefault 4;
    optimise.automatic = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  system.activationScripts.ensureBacklightPermissions = ''
    chmod a+w /sys/class/backlight/intel_backlight/brightness
  '';

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
      configurationLimit = 10;
    };
    initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sdhci_pci" ];
    plymouth.enable = true;
    extraModprobeConfig = ''
      #options iwlwifi 11n_disable=1 power_save=0
    '';
    extraModulePackages = with config.boot.kernelPackages; [ exfat-nofuse ];
    tmpOnTmpfs = true;
    kernelPackages = pkgs.linuxPackages_4_19;
    kernelParams =
      [ "scsi_mod.use_blk_mq=1" "pti=off" "nospectre_v1" "nospectre_v2" "l1tf=off" "nospec_store_bypass_disable" ];
    kernelModules = [ "bfq" "kvm-intel" ];
    kernel.sysctl = {
      "fs.inotify.max_user_instances" = 512;
      "fs.inotify.max_user_watches" = 1048576;
    };
  };

  sound.enable = true;

  environment.systemPackages = with pkgs; [
    pasystray
    pavucontrol
    # ocz-ssd-guru # add as an overlay and fix hash (and installation instructions)
    intelmetool
    me_cleaner
    smartmontools
    config.boot.kernelPackages.perf
  ];

  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  services = {
    irqbalance.enable = true;
    chrony.enable = true;
    psd = {
      enable = true;
      resyncTimer = "30min";
    };
    openssh = {
      enable = true;
      forwardX11 = true;
    };
    upower.enable = true;
    tlp.enable = true;
    acpid.enable = true;
  };

  security.sudo.wheelNeedsPassword = false;

  users.extraUsers.root.hashedPassword =
    "586c56b7b6b6f68fca29c9ff2524e4dc52d51d5b6184a65f707dd3eae075e4c9afa81c9cd4042c26c9fb773d4f3de55fb55f363c6b0f5f6790baf4c4e3f32cb9";
  nix.trustedUsers = [ "root" ];

  polkit-silent-auth.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    defaultLocale = "ru_RU.UTF-8";
    consoleUseXkbConfig = true;
    inputMethod = {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [
        table
        table-others # for LaTeX input
        m17n
      ];
    };
  };

  time.timeZone = "Europe/Moscow";

  services.printing = {
    enable = true;
    drivers = [ pkgs.hplip ];
  };

  hardware.bluetooth.enable = true;

  nixpkgs.config.allowUnfree = true;

  networking = {
    hostName = "momcat";
    # hostId = "2ab69157";
    firewall.enable = false;
    usePredictableInterfaceNames = lib.mkForce true;
    # wlanInterfaces = {
    #     "wlan0" = { device = "wlp3s0"; };
    # };
    networkmanager = {
      enable = true;
      unmanaged = [ "interface-name:ve-*" ];
    };
  };

  system.stateVersion = "19.03";
}
