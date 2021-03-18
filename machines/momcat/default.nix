{ config, inputs, lib, pkgs, ... }:

let user = config.attributes.mainUser.name;
in {
  imports = [ ./secrets ../../modules ../../profiles/thinkpad-x230.nix "${inputs.nixos-hardware}/common/pc/ssd" ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos-root";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/nixos-boot";
    fsType = "ext2";
  };

  swapDevices = [ ];

  attributes.mainUser = {
    name = config.identity.secrets.userName;
    fullName = config.identity.secrets.fullName;
    email = config.identity.secrets.email;
  };

  ext.networking = {
    core = {
      enable = true;
      hostname = "momcat";
      hostId = "007f0101";
    };
    wireless = {
      enable = true;
      ifacesMap = { "wlan0" = { device = "wlp3s0"; }; };
    };
  };

  controlcenter.enable = true;

  workstation = {
    performance.enable = true;
    video = {
      backlight = {
        enable = true;
        redshift.latitude = config.identity.secrets.redshiftLatitude;
        redshift.longitude = config.identity.secrets.redshiftLongitude;
      };
      opengl.enable = true;
    };
    power.mgmt = {
      enable = true;
      laptop.enable = true;
    };
    sound.pa = {
      enable = true;
      daemonConfig = { flat-volumes = "no"; };
    };
    input.core = {
      enable = true;
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
    lockscreen.enable = true;
    randr.enable = true;
  };

  content.misc.enable = true;

  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
      configurationLimit = 10;
    };
    initrd.availableKernelModules = [ "ahci" "ehci_pci" "sdhci_pci" "usb_storage" "xhci_pci" ];
    tmpOnTmpfs = false;
    kernelPackages = pkgs.linuxPackages_5_10;
    supportedFilesystems = [ "ntfs" ];
  };

  ext.nix.core = {
    enable = true;
    shell.enable = true;
  };

  paperworks.docflow.enable = true;

  ext.security = {
    enable = true;
    pinentryFlavor = "qt";
    polkit.silentAuth = true;
  };

  time = {
    timeZone = "Europe/Moscow";
    hardwareClockInLocalTime = true;
  };

  services = {
    chrony.enable = true;
    psd = {
      enable = true;
      resyncTimer = "30min";
    };
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
      lightdm = {
        enable = true;
        background = "${inputs.nixos-artwork}/wallpapers/nix-wallpaper-mosaic-blue.png";
        greeters.mini = {
          enable = true;
          user = user;
        };
      };
      gdm.enable = false;
      job = {
        logToFile = true;
        logToJournal = true;
      };
      defaultSession = "gnome";
    };
    autoRepeatDelay = 200;
    autoRepeatInterval = 40;
    xkbOptions = "caps:none";
    layout = "us,ru";
  };

  knowledgebase = {
    enable = true;
    man.enable = true;
    info.enable = true;
  };

  users.extraUsers.${user} = {
    isNormalUser = true;
    uid = 1000;
    description = config.attributes.mainUser.fullName;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ];
  };

  appearance.colors.zenburn.enable = true;

  browsers = {
    firefox = {
      enable = true;
      isDefault = true;
    };
    chromium = {
      enable = true;
      isFallback = true;
    };
  };

  shell = { core.enable = true; };

  home-manager = {
    useGlobalPkgs = true;
    users.${user} = {
      services.unclutter.enable = true;
      home.packages = with pkgs; [ anydesk ];
      home.stateVersion = "19.09";
    };
  };
  system.stateVersion = "19.03";
}
