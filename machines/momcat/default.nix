{ config, pkgs, lib, ... }:
let
  deps = import ../../nix/sources.nix;
in
{
  imports = [
    ../../nix/setup.nix
    "${deps.home-manager}/nixos"
    ./secrets
    ../../modules
    "${deps.nixos-hardware}/common/cpu/intel/sandy-bridge"
    "${deps.nixos-hardware}/common/pc/ssd"
    "${deps.nixos-hardware}/lenovo/thinkpad/x230"
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
    bluetooth = {
      enable = true;
      powerOnBoot = false;
    };
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
    ksm.enable = true;
    sensor.iio.enable = true;
    trackpoint = {
      enable = true;
      sensitivity = 255;
      speed = 200;
      emulateWheel = true;
    };
  };

  custom.power-management = {
    enable = true;
  };

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
    kernelModules = [ "bfq"  "thinkpad_acpi" "thinkpad_hwmon" ];
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

  custom.packaging.enable = true;

  environment.shells = with pkgs; [ "${bash}/bin/bash" "${zsh}/bin/zsh" ];

  users.extraUsers.root.hashedPassword =
    "586c56b7b6b6f68fca29c9ff2524e4dc52d51d5b6184a65f707dd3eae075e4c9afa81c9cd4042c26c9fb773d4f3de55fb55f363c6b0f5f6790baf4c4e3f32cb9";
  nix.trustedUsers = [ "root" config.attributes.mainUser.name ];

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
    arbtt.enable = true;
  };

  services.xserver = {
    enable = true;
    startDbusSession = true;
    videoDrivers = [ "modesetting" ];
    useGlamor = true;
    exportConfiguration = true;
    desktopManager = {
      xterm.enable = false;
      gnome3.enable = true;
      default = "none";
    };
    displayManager = {
      lightdm.enable = true;
      gdm.enable = false;
    };
    xkbOptions = "caps:none";
    layout = "us,ru";
    libinput.enable = false;
    multitouch = {
      enable = true;
      invertScroll = true;
      ignorePalm = true;
      tapButtons = false;
      additionalOptions = ''
        Option        "ButtonIntegrated" "true"
        Option        "ButtonMoveEmulate" "false"
        Option        "ClickTime" "25"
        Option        "EdgeBottomSize" "5"
        Option        "FingerHigh" "5"
        Option        "FingerLow" "1"
        Option        "Hold1Move1StationaryMaxMove" "1000"
        Option        "IgnoreThumb" "true"
        Option        "ScrollCoastDuration" "600"
        Option        "ScrollCoastEnableSpeed" "0.05"
        Option        "ScrollDistance" "100"
        Option        "ScrollSensitivity" "0"
        Option        "Sensitivity" "0.3"
        Option        "SwipeDistance" "700"
        Option        "SwipeDownButton" "0"
        Option        "SwipeLeftButton" "8"
        Option        "SwipeRightButton" "9"
        Option        "SwipeUpButton" "0"
        Option        "TapButton4" "0"
        Option        "ThumbRatio" "70"
        Option        "ThumbSize" "25"
      '';
    };
  };

  programs.light.enable = true;

  documentation = {
    enable = true;
    man.enable = true;
    info.enable = true;
  };

  system.stateVersion = "19.03";

  attributes.mainUser = {
    name = config.secrets.identity.userName;
    fullName = config.secrets.identity.fullName;
    email = config.secrets.identity.email;
  };

  users.extraUsers."${config.attributes.mainUser.name}" = {
    isNormalUser = true;
    uid = 1000;
    description = config.secrets.identity.fullName;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ];
  };

  appearance = {
    enable = true;
    fonts = {
      antialias = true;
      console = "Lat2-Terminus16";
    };
  };

  browsers = {
    enable = true;
    firefox.enable = true;
    chromium.enable = true;
  };

  media = {
    enable = true;
    pulse = {
        enable = true;
        daemonConfig = {
          flat-volumes = "no";
        };
    };
    opengl.enable = true;
  };

  polkit-silent-auth.enable = true;

  xinput = {
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

  themes.zenburn.enable = true;

  home-manager.users."${config.attributes.mainUser.name}" = {
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
    services.compton = {
      enable = true;
      fade = true;
      fadeDelta = 5;
      fadeSteps = [ "0.04" "0.04" ];
      backend = "glx";
      vSync = "opengl-swc";
      package = pkgs.compton-git;
      opacityRule = [ "70:class_g = 'Alacritty'" ];
      extraOptions = ''
        clear-shadow = true;
        glx-no-rebind-pixmap = true;
        glx-no-stencil = true;
        paint-on-overlay = true;
        xrender-sync-fence = true;
      '';
    };
    home.packages = with pkgs; [
      anydesk
    ];
  };
}
