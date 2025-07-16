{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

# <[editorconfig setup reference]> - <rg "editorconfig" "everything" "/home/alex3rd/workspace/repos/github.com/wiedzmin/nixos-config/">

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
    extraGroups = [ "wheel" ];
  };

  users.mutableUsers = false;
  users.users = {
    alex3rd.hashedPassword = "$6$HLpUj6dqqC6w$k5k1Pwl9Iwj/vrDmETBFHJWc0pGIL4fkMn7R2PU/ao4ydByo0yBVcJw84J2fb9ha.P0Dk2ccN5MRnDjFDY1FG.";
    root.hashedPassword = "$6$JdtKiDVrmuxSR$FYDY.JTLsNr73O0XSjBSs3YY/4FdtqizTig1RELdm1NQSwqwN7nYpLNNXmPaVcGL265uKVCrN71S/9gOIAA6C.";
  };

  attributes.hardware.monitors.internalHead.edid =
    "00ffffffffffff0026cfe5040000000000180104951c10780a123091565392281e505400000001010101010101010101010101010101201c56865000203008088800149b10000019801656865000203008088800149b10000019000000fe00496e666f566973696f6e0a2020000000fe004d3132354e575233205230200a00f1";

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
        configurationLimit = 5;
      };
    };
    initrd.availableKernelModules = [ "ahci" "ehci_pci" "sdhci_pci" "usb_storage" "xhci_pci" ];
    tmp.useTmpfs = false;
    kernelPackages = pkgs.linuxPackages_6_14;
    supportedFilesystems = [ "ntfs" ];
  };

  time = {
    timeZone = "Europe/Moscow";
    hardwareClockInLocalTime = false;
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
    locate.enable = true;
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
    };
    autoRepeatDelay = 200;
    autoRepeatInterval = 40;
    xkb = {
      options = "caps:none";
      layout = "us,ru";
    };
  };
  services.displayManager = {
    logToFile = true;
    logToJournal = true;
  };

  job = {
    "52604ab078".secrets.enable = true;
    "8286b572a1".secrets.enable = false;
  };
  dev.secrets.enable = true;

  controlcenter = {
    enable = true;
    notifications.backend = "dunst";
    clipboard.enable = true;
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
      iconSet = "all-the-icons";
    };
    fonts = {
      enable = true;
      antialias = true; # NOTE: use `nix-index consolefonts` to search values for `console.font`
      jetbrains-mono.enable = true;
    };
    wallpaper = {
      enable = true;
      boot.splashImage = "${inputs.nixos-artwork}/wallpapers/nix-wallpaper-mosaic-blue.png";
      rootDir = "${inputs.nixos-artwork}/wallpapers";
      current = "nix-wallpaper-binary-blue.png";
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
  attributes.downloadPath.browser = homePrefix user "Downloads";
  attributes.downloadPath.telegram = "${config.attributes.downloadPath.browser}/Telegram Desktop";
  attributes.debug.useLocalGoBinaries = true;

  content = {
    core.enable = true;
    images.enable = true;
    ebooks = {
      enable = true;
      emacs.pdf-tools.enable = true;
      wm.enable = true;
    };
    media = {
      enable = true;
      mpd = {
        enable = true;
        collections = { "mongol" = homePrefix user "blobs/music/mongol"; };
      };
      mpv.osc.enable = true;
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
    expansions = {
      enable = true;
      espanso.matches = {
        # FIXME: should we use custom `ext.networking.hostname` option, or stick with nixos one?
        "${config.networking.hostName}" = {
          matches = [
            {
              trigger = ":un";
              replace = user;
            }
          ];
        };
      };
    };
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
      emacs.enable = false;
    };
    vcs = {
      enable = true;
      batch.enable = true;
      ghq.enable = true;
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
        pager = "riff";
        emacs.enable = true;
      };
      misc = {
        enable = true;
      };
      navigation = {
        enable = true;
        emacs.enable = true;
      };
    };
    navigation.projects = {
      enable = true;
      bookmarks.enable = true;
      fuzzySearch.enable = true;
      emacs.enable = true;
      wm.enable = true;
    };
    misc = {
      enable = true;
      patching.enable = true;
      diagrams.enable = true;
      tools.xserver.enable = true;
      tools.misc.enable = true;
      just.chooserCmd = "fzf";
      emacs.enable = true;
      emacs.lsp.impl = "lsp-mode";
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
      clojure.enable = true;
    };
  };

  gc = {
    enable = true;
    expired = {
      enable = true;
      calendarTimespec = "*-*-* 23:30:00";
    };
    fsDeduplication.enable = true;
  };

  ide.emacs = {
    core = {
      enable = true;
      daemon.enable = true;
      pgtk.enable = false;
      wm.enable = true;
      emacsEverywhere.enable = true;
      remapEverywhere.enable = true;
      extraConfigureFlags = [ "--disable-gc-mark-trace" ];
    };
    edit.enable = true;
    navigation = {
      enable = true;
      projects.backend = "projectile";
    };
    completion = {
      enable = true;
      backend = "corfu";
      snippets.backend = "yasnippet";
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
      whatsapp.enable = true;
      emacs.enable = true;
    };
    hosts.enable = true;
    secrets = {
      enable = true;
      vpn.enable = false;
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
      permittedInsecurePackages = [
        "archiver-3.5.1"
        "beekeeper-studio-5.2.9"
        "electron-27.3.11"
        "dotnet-sdk-6.0.428"
        "beekeeper-studio-5.2.12"
      ];
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
    navigation = {
      enable = true;
      emacs.enable = true;
    };
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
    core = {
      enable = true;
      emacs.automation.enable = true;
      wm.enable = true;
    };
    orgmode.enable = true;
    scheduling = {
      enable = true;
      factoryCal.updateTimespec = "*-*-* 06:00:00";
      emacs.enable = true;
    };
    timetracking.enable = true;
  };

  workstation = {
    systemtraits.enable = true;
    backups = {
      enable = true;
      emacs.enable = true;
    };
    power = {
      mgmt = {
        enable = true;
        laptop.enable = true;
        wm.enable = true;
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
    sound = {
      enable = true;
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
            <Multi_key> <Down> : "ө"
            <Multi_key> <Up> : "Ө"
            <Multi_key> <Left> : "ү"
            <Multi_key> <Right> : "Ү"
          '';
        };
      };
      keyboard = {
        enable = true;
        remappingTool = "xremap";
        xremap.watch = true;
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
    pinentry.package = pkgs.pinentry-qt;
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
      autostart = false;
      wm.enable = true;
    };
    twopanes = {
      enable = true;
      wm.enable = true;
    };
    fish.enable = true;
  };

  ext.virtualization.core.enable = true;
  ext.virtualization.virtualbox.enable = false;

  dbms = {
    misc = {
      enable = true;
      controlCenter.enable = true;
    };
    mysql.enable = true;
    pgsql.enable = true;
  };

  dev.direnv = {
    enable = true;
    hideEnvDiff = true;
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
    mouseFocus.fracY = 0.2;
    windowFocus.impl = "wmfocus";
    gaps = {
      enable = false;
      inner.size = 5;
      outer.size = 10;
    };
    statusbar = {
      impl = "i3-rs";
      i3-rs.iconset = "material-nf";
    };
    emacs.enable = true;
  };
  wm.awesome = {
    enable = false;
    luaModules = with pkgs; [
      luaPackages.luafilesystem
      luaPackages.vicious
      nurpkgs.awesome-ezconfig
      nurpkgs.awesome-hints
      nurpkgs.awesome-lain
    ];
  };
  wm.qtile = {
    enable = false;
  };
  wm.herbstluft = {
    enable = true;
  };

  wmCommon.workspaces = [
    {
      # [tag:desktop_main]
      name = "main";
      key = [ "F2" ];
      transient = false;
      type = "primary";
    }
    {
      # [tag:desktop_tools]
      name = "tools";
      key = [ "F9" ];
      transient = false;
      type = "primary";
    }
    {
      # [tag:desktop_scan]
      name = "scan";
      key = [ "F10" ];
      transient = false;
      type = "primary";
    }
    {
      # [tag:desktop_var]
      name = "var";
      key = [ "Escape" ];
      transient = false;
      type = dockableSecondaryWS config.attributes.hardware.monitors.count;
    }
    {
      # [tag:desktop_read]
      name = "read";
      key = [ "F7" ];
      transient = false;
      type = "secondary";
    }
    {
      # [tag:desktop_im]
      name = "im";
      key = [ "c" ];
      transient = false;
      type = "secondary";
    }
    {
      # [tag:desktop_scratch]
      name = "scratch";
      key = [ "h" ];
      transient = false;
      type = "tertiary";
    }
    {
      # [tag:desktop_sandbox]
      name = "sandbox";
      key = [ "8" ];
      transient = false;
      type = "primary";
    }
  ];

  wmCommon = {
    focus.show = false;
    focus.list.useWMSpecific = true;
  };

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
