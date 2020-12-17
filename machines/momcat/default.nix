{ config, inputs, lib, pkgs, ... }:

let user = config.attributes.mainUser.name;
in {
  imports = [
    ./secrets
    ../../modules
    "${inputs.nixos-hardware}/common/cpu/intel/sandy-bridge"
    "${inputs.nixos-hardware}/common/pc/ssd"
    "${inputs.nixos-hardware}/lenovo/thinkpad/x230"
    ./filesvars.nix
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

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
    ksm.enable = true;
    sensor.iio.enable = true;
  };

  custom.power-management = { enable = true; };

  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
      configurationLimit = 10;
    };
    initrd.availableKernelModules = [ "ahci" "ehci_pci" "sdhci_pci" "usb_storage" "xhci_pci" ];
    plymouth.enable = true;
    extraModprobeConfig = ''
      options iwlwifi 11n_disable=1 power_save=1 power_level=2
    '';
    extraModulePackages = with config.boot.kernelPackages; [ exfat-nofuse ];
    tmpOnTmpfs = true;
    kernelPackages = pkgs.linuxPackages_4_19;
    kernelParams =
      [ "scsi_mod.use_blk_mq=1" "pti=off" "nospectre_v1" "nospectre_v2" "l1tf=off" "nospec_store_bypass_disable" ];
    kernelModules = [ "bfq" "thinkpad_acpi" "thinkpad_hwmon" ];
    kernel.sysctl = {
      "fs.inotify.max_user_instances" = 1024;
      "fs.inotify.max_user_watches" = 1048576;
      "fs.inotify.max_queued_events" = 32768;
      "net.ipv4.ip_default_ttl" = 65;
      "net.ipv4.tcp_sack" = 0;
    };
  };

  networking = {
    hostName = "momcat";
    hostId = "007f0101";
    firewall.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    resolvconf = {
      enable = true;
      dnsExtensionMechanism = false;
    };
    wlanInterfaces = { "wlan0" = { device = "wlp3s0"; }; };
    networkmanager.enable = false;
    wireless = {
      enable = true;
      driver = "nl80211";
      userControlled.enable = true;
      interfaces = [ "wlan0" ];
      networks = {
        "mgts161".pskRaw = "e01f873374ae7edfb0b00767e722a544b83b1127ab439ea835e087969a9e8e0c";
        "dent guest".pskRaw = "d7c9d6ecb87d5791e21a50d51ad06d8756a02e85185cc74ccba2c7b219b9daf4";
        "E-HOME".pskRaw = "55a2afc011508c7ebafc06207e03217b57bd839aa82b46819d74ac532c849e98";
        "emobile".pskRaw = "34bc8341ed02ed5efa2da222f2a93bacc3e75f76dc635c9ddc5b852ba421c857";
        "RT-WiFi_6908".pskRaw = "acb8748ab0c195789d959d11268f8802865d082544fe077ff0d34b9da87603e7";
      };
    };
    useDHCP = true;
    nameservers = [ "77.88.8.8" "77.88.8.1" "8.8.8.8" ];
  };

  custom.packaging = {
    enable = true;
    misc.enable = true;
    scripts.enable = true;
  };

  custom.paperworks.publishing.enable = true;

  environment.shells = with pkgs; [ "${bash}/bin/bash" "${zsh}/bin/zsh" ];

  nix.trustedUsers = [ "root" config.user ];

  security = {
    sudo.wheelNeedsPassword = false;
    allowUserNamespaces = true;
    allowSimultaneousMultithreading = true;
    lockKernelModules = false;
  };

  time = {
    timeZone = "Europe/Moscow";
    hardwareClockInLocalTime = true;
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
    tlp.enable = true;
    acpid.enable = true;
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "modesetting" ];
    useGlamor = true;
    exportConfiguration = true;
    desktopManager = {
      xterm.enable = false;
      gnome3.enable = true;
    };
    displayManager = {
      lightdm.enable = true;
      gdm.enable = false;
      defaultSession = "gnome";
    };
    xkbOptions = "caps:none";
    layout = "us,ru";
  };

  programs.light.enable = true;

  documentation = {
    enable = true;
    man.enable = true;
    info.enable = true;
  };

  attributes.mainUser = {
    name = config.identity.secrets.userName;
    fullName = config.identity.secrets.fullName;
    email = config.identity.secrets.email;
  };

  users.extraUsers.${user} = {
    isNormalUser = true;
    uid = 1000;
    description = config.identity.secrets.fullName;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ];
  };

  custom.appearance = {
    enable = true;
    fonts = {
      antialias = true;
      console = "Lat2-Terminus16";
    };
  };

  custom.browsers = {
    firefox = {
      enable = true;
      isDefault = true;
    };
    chromium = {
      enable = true;
      isFallback = true;
    };
  };

  custom.sound = {
    enable = true;
    pulse = {
      enable = true;
      daemonConfig = { flat-volumes = "no"; };
    };
  };

  custom.security = {
    enable = true;
    pinentryFlavor = "qt";
    polkit.silentAuth = true;
  };

  custom.xinput = {
    hardware.enable = true;
    xmodmap = {
      enable = true;
      rc = ''
        clear mod1
        clear mod4
        clear mod5
        keycode 64 = Alt_L Meta_L
        keycode 133 = Super_L
        keycode 108 = Hyper_L
        keycode 191 = Insert
        add mod1 = Meta_L
        add mod1 = Alt_L
        add mod4 = Super_L
        add mod5 = Hyper_L
      '';
    };
  };

  custom.video = {
    enable = true;
    opengl.enable = true;
    autorandr.enable = true;
    screenlocker.enable = true;
  };

  themes.zenburn.enable = true;

  home-manager = {
    useGlobalPkgs = true;
    users.${user} = {
      services.unclutter.enable = true;
      services.udiskie.enable = true;
      programs.htop.enable = true;
      programs.command-not-found.enable = true;
      programs.lesspipe.enable = true;
      programs.fzf = {
        enable = true;
        enableZshIntegration = true;
      };
      programs.direnv = {
        enable = true;
        enableZshIntegration = true;
      };
      home.packages = with pkgs; [ anydesk ];

      home.stateVersion = "19.09";
    };
  };

  system.stateVersion = "19.03";
}
