{ config, inputs, lib, pkgs, ... }:
with import ../../modules/util.nix { inherit config inputs lib pkgs; };

let
  user = config.attributes.mainUser.name;
  stable = import inputs.stable ({
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  });
in
{
  imports =
    [ "${inputs.nixos-hardware}/common/pc/ssd" ../../modules ../../profiles/thinkpad-x270.nix ./assets ./secrets ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos-root";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/nixos-boot";
    fsType = "ext2";
  };

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-partlabel/efi-system";
    fsType = "vfat";
  };

  swapDevices = [ ];

  environment.etc.current-configuration.source = ../../.;

  attributes.machine.name = "laptoptop";
  attributes.mainUser = {
    name = config.identity.secrets.userName;
    ID = builtins.toString config.users.extraUsers."${config.identity.secrets.userName}".uid;
    fullName = config.identity.secrets.fullName;
    email = config.identity.secrets.email;
    gpgKeyID = config.identity.secrets.gpgKeyID;
  };

  users.extraUsers.${user} = {
    isNormalUser = true;
    uid = 1000;
    description = config.attributes.mainUser.fullName;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ];
  };

  attributes.hardware.monitors = {
    internalHead.name = "eDP-1";
    internalHead.edid =
      "00ffffffffffff0030e4a3040000000000190104951c10780a28b597595492261e505400000001010101010101010101010101010101961d56e85000163030202500159c10000019000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d53505432003f";
    internalHead.resolution = "1366x768";
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi = {
        # canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      grub = {
        enable = true;
        efiInstallAsRemovable = true;
        # devices = [ "nodev" ];
        efiSupport = true;
        device = "/dev/sda";
        configurationLimit = 30;
        version = 2;
      };
    };
    initrd.availableKernelModules = [ "ahci" "ehci_pci" "sdhci_pci" "usb_storage" "xhci_pci" ];
    tmpOnTmpfs = false;
    kernelPackages = pkgs.linuxPackages_5_10;
    supportedFilesystems = [ "ntfs" ];
  };

  time = {
    timeZone = "Europe/Moscow";
    hardwareClockInLocalTime = true;
  };

  services = {
    chrony.enable = true;
    dbus = { enable = true; };
    smartd = {
      enable = true;
      notifications = { x11.enable = true; };
    };
    logind.lidSwitchDocked = "suspend";
    journald.extraConfig = ''
      MaxRetentionSec=7day
    '';
    thermald.enable = true;
    acpid.enable = true;
    timesyncd.enable = true;
  };

  systemd.services.acme-localhost.enable = false;

  services.xserver = {
    enable = true;
    videoDrivers = [ "modesetting" ];
    useGlamor = true;
    exportConfiguration = true;
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
    };
    autoRepeatDelay = 200;
    autoRepeatInterval = 40;
    xkbOptions = "caps:none";
    layout = "us,ru";
  };

  job = {
    "52604ab078".secrets.enable = true;
    "7880d0f26f".secrets.enable = true;
    "8a1add5330".secrets.enable = true;
  };
  dev.secrets.enable = true;

  controlcenter = {
    enable = true;
    notifications.backend = "dunst";
    gmrun.enable = true;
    wm.enable = true;
  };

  appearance = {
    colors.zenburn.enable = true;
    emacs.themes.zenburn.enable = true;
    emacs = {
      enable = true;
      modeline.telephone = {
        enable = true;
        height = 24;
      };
    };
    fonts = {
      enable = true;
      antialias = true; # NOTE: use `nix-index consolefonts` to search values for `console.font`
      iosevka.enable = true;
    };
    wallpaper = {
      enable = true;
      boot.splashImage = "${inputs.nixos-artwork}/wallpapers/nix-wallpaper-mosaic-blue.png";
      rootDir = homePrefix "blobs/wallpaper/mongol/spring";
      current = "mongolia-spring-1.jpg";
      wm.enable = true;
    };
    xresources = {
      enable = true;
      wm.enable = true;
    };
  };

  browsers = {
    ext = {
      enable = true;
      emacs.enable = true;
      wm.enable = true;
    };
    qutebrowser = {
      enable = true;
      isDefault = true;
      sessions.backup.enable = true;
    };
    chromium = {
      enable = true;
      isFallback = true;
    };
    nyxt.enable = true;
  };

  content = {
    core.enable = true;
    images.enable = true;
    ebooks = {
      enable = true;
      wm.enable = true;
    };
    media = {
      enable = true;
      mpd = {
        enable = true;
        collections = { "mongol" = homePrefix "blobs/music/mongol"; };
      };
      wm.enable = true;
    };
    misc = {
      enable = true;
      wm.enable = true;
    };
    screenshots = {
      enable = true;
      baseDir = homePrefix "blobs/screenshots";
      ordering = {
        enable = true;
        timespec = "*-*-* 00:05:00";
      };
      wm.enable = true;
    };
  };

  completion = {
    enable = true;
    expansions.enable = true;
    shell.enable = true;
    emacs.enable = true;
    wm.enable = true;
  };

  dev = {
    codesearch = {
      enable = true;
      emacs.enable = true;
    };
    projectenv.enable = true;
    git = {
      autofetch = {
        enable = false;
        when = "hourly";
      };
      autopush = {
        enable = false;
        when = "*-*-* 18:00:00";
      };
      savewip = {
        enable = false;
        when = "hourly";
      };
      batch.enable = true;
      forges = {
        enable = true;
        emacs.enable = true;
      };
      core = {
        enable = true;
        emacs.enable = true;
      };
      misc = {
        enable = true;
        emacs.enable = true;
      };
      navigation = {
        enable = true;
        ghq.enable = true;
      };
    };
    navigation.projects = {
      enable = true;
      bookmarks.enable = true;
      fuzzySearch.enable = true;
      wm.enable = true;
    };
    misc = {
      enable = true;
      patching.enable = true;
      tools.xserver.enable = true;
      tools.misc.enable = true;
      emacs.enable = true;
    };
    python = {
      enable = true;
      emacs.enable = true;
    };
    ccpp = {
      enable = true;
      emacs.enable = true;
    };
    golang = {
      enable = true;
      goPath = homePrefix "workspace/go";
      misc.enable = true;
      emacs.enable = true;
    };
  };

  gc = {
    enable = true;
    trash = {
      enable = true;
      calendarTimespec = "*-*-* 23:00:00";
    };
    expired = {
      enable = true;
      calendarTimespec = "*-*-* 23:30:00";
    };
    fsDeduplication.enable = true;
  };

  ide.emacs = {
    core = {
      enable = true;
      native.enable = true;
      wm.enable = true;
    };
    edit.enable = true;
    navigation.enable = true;
    history.enable = true;
    misc.enable = true;
  };

  knowledgebase = {
    enable = true;
    emacs.enable = true;
  };

  navigation = {
    bookmarks = {
      enable = true;
      workspaces.globalRoot = "workspace/repos";
      emacs.enable = true;
    };
  };

  systemd.services.NetworkManager-wait-online.enable = false;

  ext.networking = {
    core = {
      enable = true;
      hostId = "2ab69157";
    };
    messengers = {
      enable = true;
      telegram.autostart = true;
      emacs.enable = true;
    };
    hosts.enable = true;
    secrets = {
      enable = true;
      vpn.enable = true;
      wifi.enable = true;
    };
    nmconnections.enable = true;
    wireless = {
      enable = true;
      ifacesMap = { "wlan0" = { device = "wlp3s0"; }; };
      macAddress = "60:67:20:ec:34:14";
      bluetooth = {
        enable = true;
        headsetMacAddresses = config.ext.networking.secrets.headsetMacAddresses;
      };
      wm = {
        enable = true;
        dmenu.enable = true;
      };
    };
    ssh = {
      enable = true;
      keypair = {
        private = config.identity.secrets.ssh.privateKey;
        public = config.identity.secrets.ssh.publicKey;
      };
      authorizedKeys = [ (secretsPrefix "identity/id_rsa.mobile.pub") ];
      wm.enable = true;
    };
    vpn = {
      enable = true;
      wm.enable = true;
    };
  };

  ext.nix = {
    core = {
      enable = true;
      emacs.enable = true;
    };
    cachix = {
      enable = true;
      username = user;
      configuration = builtins.readFile ./secrets/cachix.dhall;
    };
    dev = {
      enable = true;
      scripts.enable = true;
    };
    navigation.enable = true;
  };

  paperworks = {
    printing = {
      enable = true;
      drivers = [ stable.hplip ];
    };
    scanning = {
      enable = true;
      extraBackends = [ stable.hplipWithPlugin ];
      paperless = {
        enable = false;
        consumptionDir = homePrefix "docs/paperless/consume";
        dataDir = homePrefix "docs/paperless/data";
        user = user;
        extraConfig = { PAPERLESS_FORGIVING_OCR = true; };
        group = "users";
      };
    };
    docflow.enable = true;
    processors.enable = true;
  };

  pim = {
    orgmode.enable = true;
    scheduling = {
      enable = true;
      emacs.enable = true;
    };
    timetracking.enable = true;
  };

  workstation = {
    systemtraits.enable = true;
    power = {
      mgmt = {
        enable = true;
        laptop.enable = true;
        commands.resume = lib.concatStringsSep "\n"
          (lib.mapAttrsToList (server: _: "${pkgs.systemd}/bin/systemctl try-restart openvpn-${server}.service")
            config.services.openvpn.servers);
        commands.suspend = ''
          redis-cli --scan --pattern "*is_up" | xargs redis-cli del
        '';
      };
      battery = {
        enable = true;
        dischargeNotificationPercents = 20;
        suspension.percents = 10;
      };
    };
    performance = {
      enable = true;
      wm.enable = true;
    };
    lockscreen = {
      enable = true;
      notification.timeout = 5000;
      timers.alert = 210;
      timers.lock = 60;
      wm.enable = true;
    };
    sound.pa = {
      enable = true;
      daemonConfig = { flat-volumes = "no"; };
      wm.enable = true;
    };
    randr = {
      enable = true;
      heads.orientation.secondary = "normal";
      wm.enable = true;
    };
    input = {
      core = {
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
            keysym Alt_R = Multi_key
          '';
        };
        xcompose = {
          enable = true;
          mappings = ''
            # Mongolian-specific
            <Multi_key> <m> <o> : "ө"
            <Multi_key> <m> <O> : "Ө"
            <Multi_key> <m> <u> : "ү"
            <Multi_key> <m> <U> : "Ү"
          '';
        };
      };
      xkeysnail.enable = true;
      mouse = {
        constraintMouse = { enable = false; };
        keynav.enable = true;
      };
    };
    video = {
      backlight = {
        enable = true;
        redshift.latitude = config.identity.secrets.redshiftLatitude;
        redshift.longitude = config.identity.secrets.redshiftLongitude;
        wm.enable = true;
      };
      transparency = {
        enable = true;
        wm.enable = true;
      };
    };
  };

  ext.security = {
    enable = true;
    pinentryFlavor = "qt";
    polkit.silentAuth = true;
    emacs.enable = true;
    wm.enable = true;
  };

  shell = {
    bookmarks.enable = true;
    core = {
      enable = true;
      dev.enable = true;
      emacs.enable = true;
    };
    prompts.starship.enable = true;
    tmux = {
      enable = true;
      theme.package = pkgs.tmuxPlugins.power-theme;
      wm.enable = true;
    };
    tools.enable = true;
    vt.alacritty = {
      enable = true;
      autostart = true;
      wm.enable = true;
    };
    twopanes = {
      enable = true;
      wm.enable = true;
    };
    zsh.enable = true;
  };

  ext.virtualization = {
    docker = {
      core = {
        enable = true;
        emacs.enable = true;
        wm.enable = true;
      };
      devdns = {
        enable = true;
        wm.enable = true;
      };
      swarm = {
        enable = true;
        wm.enable = true;
      };
    };
    libvirt.enable = true;
    virtualbox.enable = false;
  };

  dbms = {
    misc = {
      enable = true;
      controlCenter.enable = true;
      wm.enable = true;
    };
    mysql.enable = true;
    pgsql.enable = true;
  };

  dev.direnv = {
    enable = true;
    emacs.enable = true;
    emacs.granularity = "file";
  };

  dev.editorconfig = {
    enable = true;
    emacs.enable = true;
  };

  wm.i3 = {
    enable = true;
    statusbar.impl = "i3-rs";
  };

  wmCommon.workspaces = [
    {
      name = "web";
      key = [ "F1" ];
      transient = false;
      type = "primary";
    }
    {
      name = "edit";
      key = [ "F2" ];
      transient = false;
      type = "primary";
    }
    {
      name = "tools";
      key = [ "F4" ];
      transient = false;
      type = "primary";
    }
    {
      name = "scan";
      key = [ "F5" ];
      transient = false;
      type = "primary";
    }
    {
      name = "ent";
      key = [ "F6" ];
      transient = false;
      type = "primary";
    }
    {
      name = "shell";
      key = [ "F3" ];
      transient = false;
      type = "secondary";
    }
    {
      name = "read";
      key = [ "4" ];
      transient = false;
      type = "secondary";
    }
    {
      name = "im";
      key = [ "c" ];
      transient = false;
      type = "secondary";
    }
    {
      name = "scratch";
      key = [ "Esc" ];
      transient = false;
      type = "tertiary";
    }
  ];

  wmCommon.showfocus = false;

  home-manager = {
    useGlobalPkgs = true;
    users."${user}" = {
      xdg.enable = true;
      home.stateVersion = "20.09";
    };
  };

  system.stateVersion = "20.09";
}
