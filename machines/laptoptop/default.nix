{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  imports = [
    "${inputs.nixos-hardware}/common/pc/ssd"
    ../../modules
    ../../profiles/chassis/thinkpad-x270.nix
    ../../profiles/chassis/40A10090EU.nix
    ./assets
    ./secrets
  ];

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

  users.extraUsers."${user}" = {
    isNormalUser = true;
    uid = 1000;
    description = config.attributes.mainUser.fullName;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ];
  };
  programs.zsh.enable = true;

  users.mutableUsers = false;
  users.users = {
    alex3rd.hashedPassword = "$6$HLpUj6dqqC6w$k5k1Pwl9Iwj/vrDmETBFHJWc0pGIL4fkMn7R2PU/ao4ydByo0yBVcJw84J2fb9ha.P0Dk2ccN5MRnDjFDY1FG.";
    root.hashedPassword = "$6$JdtKiDVrmuxSR$FYDY.JTLsNr73O0XSjBSs3YY/4FdtqizTig1RELdm1NQSwqwN7nYpLNNXmPaVcGL265uKVCrN71S/9gOIAA6C.";
  };

  attributes.hardware.monitors.internalHead.edid =
    "00ffffffffffff0030e4a3040000000000190104951c10780a28b597595492261e505400000001010101010101010101010101010101961d56e85000163030202500159c10000019000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d53505432003f";

  boot = {
    loader = {
      systemd-boot.enable = false;
      efi = {
        efiSysMountPoint = "/boot/efi";
      };
      grub = {
        enable = true;
        efiInstallAsRemovable = true;
        efiSupport = true;
        device = "/dev/sda";
        configurationLimit = 50;
      };
    };
    initrd.availableKernelModules = [ "ahci" "ehci_pci" "sdhci_pci" "usb_storage" "xhci_pci" ];
    tmp.useTmpfs = false;
    kernelPackages = pkgs.linuxPackages_5_15;
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
    "8286b572a1".secrets.enable = true;
  };
  dev.secrets.enable = true;

  controlcenter = {
    enable = true;
    notifications.backend = "dunst";
    wm.enable = true;
  };

  appearance = {
    colors.zenburn.enable = true;
    emacs.themes.zenburn.enable = true;
    emacs = {
      enable = true;
      currentLineHighlightFace = "dark slate blue";
      modeline.doom = {
        enable = false;
        height = 24;
        displayMinorModes = true;
      };
      modeline.telephone.enable = true;
    };
    fonts = {
      enable = true;
      antialias = true; # NOTE: use `nix-index consolefonts` to search values for `console.font`
      jetbrains-mono.enable = true;
    };
    wallpaper = {
      enable = true;
      boot.splashImage = "${inputs.nixos-artwork}/wallpapers/nix-wallpaper-mosaic-blue.png";
      rootDir = homePrefix user "blobs/wallpaper/mongol/autumn";
      current = "forest_pano.jpg";
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
    chromium = {
      enable = true;
      suspendInactive = false;
      isDefault = false;
      isFallback = true;
    };
    qutebrowser = {
      enable = true;
      emacsKeys.enable = true;
      isDefault = true;
      suspendInactive = false;
      sessions = {
        backup.enable = true;
      };
    };
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
        collections = { "mongol" = homePrefix user "blobs/music/mongol"; };
      };
      wm.enable = true;
    };
    misc = {
      enable = true;
      syncthing.enable = false;
      emacs.enable = true;
      wm.enable = true;
    };
    screenshots = {
      enable = true;
      baseDir = homePrefix user "blobs/screenshots";
      ordering = {
        enable = true;
        timespec = "*-*-* 00:05:00";
      };
      wm.enable = true;
    };
  };

  completion = {
    expansions.enable = true;
    tabnine = {
      enable = true;
      emacs = {
        enable = true;
        package = "company-tabnine";
      };
    };
  };

  history = {
    shell = {
      enable = true;
      backend = "atuin";
      mcfly = {
        fuzzySearch = 3;
        resultsCount = 30;
        resultsSort = "LAST_RUN";
      };
    };
    emacs.enable = true;
  };

  dev = {
    codesearch = {
      enable = true;
      emacs.enable = true;
    };
    vcs = {
      enable = true;
      batch.enable = true;
      emacs.enable = true;
    };
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
      };
      navigation = {
        enable = true;
        emacs.enable = true;
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
      just.chooserCmd = "fzf";
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
      goPath = homePrefix user "workspace/go";
      misc.enable = true;
      emacs.enable = true;
    };
    ml = {
      enable = true;
      emacs.enable = true;
    };
    lisp = {
      cl.enable = false; # FIXME: slime. Also check ability to pin particular epkgs
      elisp.enable = true;
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
      pgtk.enable = false;
      wm.enable = true;
      emacsEverywhere.enable = true;
    };
    edit.enable = true;
    navigation = {
      enable = true;
      projects.backend = "projectile";
    };
    completion = {
      enable = true;
      backend = "company";
      snippets.backend = "tempel";
    };
    misc.enable = true;
  };

  knowledgebase = {
    enable = true;
    emacs.enable = true;
  };

  navigation = {
    bookmarks = {
      enable = true;
      workspaces = {
        globalRoot = homePrefix user "workspace/repos";
        globalRootStale = homePrefix user "workspace/stale";
      };
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
        devices = {
          "MiAir2" = "6C:CE:44:AE:97:39";
          "BO9" = "1A:2B:F5:5E:BE:63";
        };
        defaultHeadset = "MiAir2";
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
      authorizedKeys = [
        (secretsPrefix
          config.attributes.machine.name "identity/id_rsa.mobile.pub"
          config.navigation.bookmarks.workspaces.roots)
      ];
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
      treesitter.enable = false;
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
      drivers = [ pkgs.hplip ];
    };
    scanning = {
      enable = true;
      frontend = "xsane";
      extraBackends = [ pkgs.epkowa ];
      snapscan = {
        enable = true;
        firmware = ../../modules/paperworks/assets/Esfw52.bin;
      };
      paperless = {
        enable = false;
        consumptionDir = homePrefix user "docs/paperless/consume";
        dataDir = homePrefix user "docs/paperless/data";
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
      oom.enable = true;
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
      heads.orientation.primary = "normal";
      heads.orientation.secondary = "normal";
      wm.enable = true;
    };
    input = {
      core = {
        enable = true;
        xmodmap = {
          enable = false;
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
            <Multi_key> <minus> <Down> : "ө"
            <Multi_key> <minus> <Up> : "Ө"
            <Multi_key> <minus> <Left> : "ү"
            <Multi_key> <minus> <Right> : "Ү"
          '';
        };
      };
      keyboard = {
        enable = true;
        remappingTool = "xkeysnail";
      };
      mouse = {
        enable = true;
        constraintMouse = { enable = false; };
        keynavTool = "warpd";
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
        enable = false;
        wm.enable = false;
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
      queueing.enable = true;
      emacs.enable = true;
    };
    prompts.starship.enable = true;
    tools = {
      enable = true;
      pager = "moar";
    };
    vt.kitty = {
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
        aux.enable = true;
        emacs.enable = true;
        wm.enable = true;
      };
      devdns = {
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
    isDefault = true;
    titleAlignment = "center";
    focusOnWindowActivation = "urgent";
    gaps = {
      enable = false;
      inner.size = 5;
      outer.size = 10;
    };
    statusbar.impl = "i3-rs";
    windowFocus.fontSize = 55;
    emacs.enable = true;
  };
  wm.awesome = {
    enable = true;
    luaModules = with pkgs; [
      luaPackages.luafilesystem
      luaPackages.vicious
      nurpkgs.awesome-ezconfig
      nurpkgs.awesome-hints
      nurpkgs.awesome-lain
    ];
  };
  wm.qtile = {
    enable = true;
  };
  wm.xmonad = {
    enable = true;
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
      type = dockablePrimaryWS config.attributes.hardware.monitors.count;
    }
    {
      name = "tools";
      key = [ "1" ];
      transient = false;
      type = "primary";
    }
    {
      name = "scan";
      key = [ "2" ];
      transient = false;
      type = "primary";
    }
    {
      name = "var";
      key = [ "Escape" ];
      transient = false;
      type = dockableSecondaryWS config.attributes.hardware.monitors.count;
    }
    {
      name = "shell";
      key = [ "F3" ];
      transient = false;
      type = "secondary";
    }
    {
      name = "read";
      key = [ "3" ];
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
      key = [ "h" ];
      transient = false;
      type = "tertiary";
    }
  ];

  wmCommon.focus.show = false;

  home-manager = {
    useGlobalPkgs = true;
    users."${user}" = {
      home.packages = with pkgs; [ xkb-switch ] ++ [ nurpkgs.dmenu-ng rofi /* NOTE: temp, until publishing upstream */ ];
      xdg.enable = true;
      home.stateVersion = "22.11";
    };
  };

  system.stateVersion = "22.09";
}
