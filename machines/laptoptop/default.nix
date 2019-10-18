{ config, pkgs, lib, ... }:

let
  custom = import ../../pkgs/custom pkgs config;
in {
  imports = [
    ../../pkgs/setup.nix
    <home-manager/nixos>
    ./secrets
    ../../modules
    ../../pkgs/forges/github.com/NixOS/nixos-hardware/common/cpu/intel/sandy-bridge
    ../../pkgs/forges/github.com/NixOS/nixos-hardware/common/pc/ssd
    ../../pkgs/forges/github.com/NixOS/nixos-hardware/lenovo/thinkpad/x230
    ./filesvars.nix
    ./emacs
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

  fileSystems."${config.services.syncthing.dataDir}/bookshelf" = {
    device =
      "${config.users.extraUsers.alex3rd.home}/bookshelf"; # TODO: check if we could use env var or substitution here
    options = [ "bind" ];
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

  powerManagement = {
    enable = true;
    powertop.enable = true;
    resumeCommands = lib.concatStringsSep "\n"
      (lib.mapAttrsToList (server: conf: "${pkgs.systemd}/bin/systemctl try-restart openvpn-${server}.service")
        config.services.openvpn.servers);
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
    hostName = "laptoptop";
    hostId = "2ab69157";
    firewall.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    resolvconf = {
      enable = true;
      dnsExtensionMechanism = false;
    };
    wlanInterfaces = { "wlan0" = { device = "wlp3s0"; }; };
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
        "${config.secrets.job.officeSSID}".pskRaw = "2d3409526a97aabf44bbbfc3afde3acbc252843e795969915b6c830387443906";
      };
    };
    useDHCP = true;
    nameservers = [ "77.88.8.8" "77.88.8.1" "8.8.8.8" ];
    extraHosts = ''
      127.0.0.1 ${config.networking.hostName}
      ${builtins.concatStringsSep "\n"
        (lib.mapAttrsToList (ip: hosts: ip + "    " + (builtins.concatStringsSep " " hosts))
          (config.secrets.job.infra.extraHosts // config.secrets.network.extraHosts))};
    '';
  };

  nix = {
    # per-machine settings
    maxJobs = lib.mkDefault 4;
    buildCores = lib.mkDefault 4;
    optimise.automatic = false;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  nixpkgs.config = {
    allowUnfree = true;
    allowUnfreeRedistributable = true;

    oraclejdk.accept_license = true;
  };

  environment = {
    shells = with pkgs; [ "${bash}/bin/bash" "${zsh}/bin/zsh" ];
    systemPackages = with pkgs;
      with config.boot.kernelPackages;
      [
        # ocz-ssd-guru # add as an overlay and fix hash (and installation instructions)
        intelmetool
        me_cleaner
      ] ++ [ wpa_supplicant_gui ] ++ [
        perf
        cpupower
        # hotspot # rarely used
      ];
  };

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
    dbus = {
      enable = true;
      socketActivated = true;
    };
    earlyoom.enable = true;
    openssh = {
      enable = true;
      allowSFTP = true;
      forwardX11 = false;
    };
    smartd = {
      enable = true;
      notifications = { x11.enable = true; };
    };
    logind.lidSwitchDocked = "suspend";
    journald.extraConfig = ''
      MaxRetentionSec=7day
    '';
    udev.extraRules = ''
      ACTION=="add|change", KERNEL=="sd[ab][!0-9]", ATTR{queue/scheduler}="kyber"

      SUBSYSTEM=="backlight", ACTION=="add", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
      SUBSYSTEM=="leds", ACTION=="add", KERNEL=="*::kbd_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/leds/%k/brightness", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/leds/%k/brightness"

      ACTION=="change", SUBSYSTEM=="drm", RUN+="${pkgs.autorandr}/bin/autorandr --batch --change --default default"
    '';
    upower.enable = true;
    tlp.enable = true;
    thermald.enable = true;
    acpid.enable = true;
    timesyncd.enable = true;

    redis.enable = true; # for various caching needs

    xidlehook.enable = true;
    arbtt.enable = true;
    clean-trash = {
      enable = true;
      calendarTimespec = "*-*-* 23:00:00";
    };
    batteryNotifier = {
      enable = true;
      notifyCapacity = 20;
      suspendCapacity = 10;
    };
    openvpn.servers = {
      "${config.secrets.network.vpn.name}" = {
        config = config.secrets.network.vpn.config;
        autoStart = false;
        updateResolvConf = true;
        authUserPass = {
          username = config.secrets.network.vpn.username;
          password = config.secrets.network.vpn.password;
        };
      };
      "${config.secrets.job.vpn.name}" = {
        config = config.secrets.job.vpn.config;
        autoStart = true;
        updateResolvConf = true;
        authUserPass = {
          username = config.secrets.job.vpn.username;
          password = config.secrets.job.vpn.password;
        };
      };
    };
    unclutter-xfixes = {
      enable = true;
      timeout = 2;
      threshold = 15;
      extraOptions = [ "exclude-root" "fork" "ignore-scrolling" ];
    };
  };

  attributes.hardware.monitors = {
    internalHead.name = "LVDS-1";
    internalHead.edid = "00ffffffffffff0030e4d8020000000000160103801c1078ea8855995b558f261d505400000001010101010101010101010101010101601d56d85000183030404700159c1000001b000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d534c42330059";
    internalHead.resolution = "1366x768";
  };

  services.xserver = {
    enable = true;
    startDbusSession = true;
    videoDrivers = [ "modesetting" ];
    useGlamor = true;
    exportConfiguration = true;
    desktopManager = {
      xterm.enable = false;
      gnome3.enable = false;
      default = "none";
    };
    displayManager = {
      lightdm = {
        enable = true;
        background = "black";
        greeters.mini = {
          enable = true;
          user = config.attributes.mainUser.name;
        };
      };
      gdm.enable = false;
      job = {
        logToFile = true;
        logToJournal = true;
      };
      sessionCommands = ''
        export _JAVA_AWT_WM_NONREPARENTING=1
        ${pkgs.wmname}/bin/wmname LG3D
      '';
    };
    autoRepeatDelay = 200;
    autoRepeatInterval = 40;
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
    doc.enable = true;
    dev.enable = true;
    nixos = {
      enable = true;
      includeAllModules = false; # FIXME build error
    };
  };

  system.stateVersion = "19.03";

  attributes.mainUser = {
    name = config.secrets.identity.userName;
    fullName = config.secrets.identity.fullName;
    email = config.secrets.identity.email;
    gpgKeyID = config.secrets.identity.gpgKeyID;
  };

  attributes.testLines = lib.mkAfter ''

    333
    444
  '';

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
    wallpaper = {
      enable = true;
      root = "/home/${config.attributes.mainUser.name}/blobs/wallpaper";
      current = "mongolia_2.jpg";
    };
  };

  browsers = {
    enable = true;
    firefox.enable = true;
    chromium.enable = true;
    aux.enable = true;
  };

  dev.git = {
    enable = true;
    pager.delta.enable = true;
    myrepos.enable = false; # temporarily
    myrepos.subconfigs = [
      "/home/${config.attributes.mainUser.name}/workspace/repos/.mrconfig"
    ];
    ghq = {
      enable = true;
      importCommands = {
        bbcontribs = "bitbucket_team_contributor_repos";
      };
    };
    github = {
      enable = true;
      user = "wiedzmin";
    };
    workspaceRoot = "/home/${config.attributes.mainUser.name}/workspace/repos";
    fetchUpdates.enable = false; # temporarily; bootTimespec = "1min"; activeTimespec = "30min";
    pushUpdates.enable = false; # temporarily; calendar = "*-*-* 18:00:00";
    saveWip.enable = false; # temporarily; bootTimespec = "30sec"; activeTimespec = "1hour";
  };

  dev.python.enable = true;

  dev.tools = {
    statistics.enable = true;
    misc.enable = true;
  };

  email = {
    enable = true;
    emailAddress = config.secrets.identity.email;
    passwordPath = config.secrets.identity.googleAccountPasswordPath;
    gpg = {
      sign = true;
      keyID = config.secrets.identity.gpgKeyID;
    };
    mbsync = {
      enable = true;
      postExec = "${pkgs.notmuch}/bin/notmuch new";
    };
    msmtp.enable = true;
    notmuch.enable = true;
    imapfilter = {
      enable = true;
      server = "imap.google.com";
      byFrom = config.secrets.email.imapfilter.byFrom;
      byTo = config.secrets.email.imapfilter.byTo;
      byCc = config.secrets.email.imapfilter.byCc;
      bySubject = config.secrets.email.imapfilter.bySubject;
      deleteByFrom = config.secrets.email.imapfilter.deleteByFrom;
    };
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

  messengers.enable = true;

  navigation = {
    enable = true;
    gmrun.enable = true;
    mc.enable = true;
    rofi.enable = true;
  };

  packaging = {
    enable = true;
    nix = {
      helpers.enable = true;
      srcfmt.enable = true;
    };
  };

  polkit-silent-auth.enable = true;

  screenshots = {
    enable = true;
    baseDir = "/home/${config.attributes.mainUser.name}/screenshots";
    dateFormat = "+%Y-%m-%d_%H:%M:%S";
    calendarTimespec = "*-*-* 00:05:00";
  };

  tools = {
    content = {
      consumers.enable = true;
      orderingTools.enable = true;
      videoTools.enable = true;
    };
    dbms = {
      mysql.enable = true;
      jobDbms.enable = true;
    };
    ebooks.readers.enable = true;
    security.enable = true;
    system = {
      forensics.enable = true;
      monitoring.enable = true;
    };
  };

  virtualization = {
    enable = true;
    docker = {
      enable = true;
      devdns.enable = true;
    };
    libvirt.enable = true;
  };

  wm.xmonad.enable = true;

  xinput = {
    constraintMouse.enable = true;
    gestures.enable = true;
    keynav.enable = true;
    xkeysnail.enable = true;
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

  xrandr = {
    enable = true;
    autorandr.enable = true;
  };

  themes.condensedFonts.enable = true;
  themes.zenburn.enable = true;

  # FIXME: move functionality to pkgsctl
  system.activationScripts.saveCurrentHMVersion = ''
    touch /etc/current-home-manager
    cd /etc/nixos/pkgs/forges/github.com/rycee/home-manager
    hm_revision="upstream: $(${pkgs.git}/bin/git rev-parse --short HEAD)"
    echo "$hm_revision" >> /etc/current-home-manager
  '';

  home-manager.users."${config.attributes.mainUser.name}" = {
    nixpkgs.config.allowUnfree = true;
    xdg.enable = true;
    home.file = {
      "test_lines".text = config.attributes.testLines;
    };
    home.packages = with pkgs; [
      fpp       # for tmux fpp plugin
      libnotify # for zsh-notify plugin
      xsel # for firefox native clients
      haskellPackages.arbtt # for stats viewing

      custom.confctl
      custom.format-config
      custom.pkgsctl
      custom.update-system

      custom.docker_stacks_info_new
    ];
    services = {
      gpg-agent = {
        enable = true;
        defaultCacheTtl = 34560000;
        defaultCacheTtlSsh = 34560000;
        maxCacheTtl = 34560000;
        enableSshSupport = true;
        enableExtraSocket = true;
        extraConfig = ''
          allow-emacs-pinentry
          allow-loopback-pinentry
        '';
      };
      syncthing.enable = true;
      mpd = {
        enable = true;
        musicDirectory = "/home/${config.attributes.mainUser.name}/blobs/music";
      };
      xsuspender = {
        enable = true;
        defaults = {
          suspendDelay = 10;
          onlyOnBattery = false;
        };
        rules = {
          Chromium = {
            suspendDelay = 10;
            matchWmClassContains = "Chromium-browser";
            suspendSubtreePattern = "chromium";
          };
          Firefox = {
            suspendDelay = 10;
            matchWmClassContains = "Firefox";
            suspendSubtreePattern = "firefox";
          };
        };
      };
      dunst = {
        enable = true;
        settings = {
          global = {
            alignment = "left";
            always_run_script = "true";
            bounce_freq = 0;
            browser = "${pkgs.firefox-unwrapped}/bin/firefox -new-tab";
            dmenu = "${pkgs.dmenu}/bin/dmenu -p dunst:";
            ellipsize = "middle";
            follow = "keyboard";
            force_xinerama = "false";
            format = "<span foreground='#F3F4F5'><b>%s %p</b></span>\\n%b";
            frame_color = "#232323";
            frame_width = 3;
            geometry = "300x5-15+15";
            hide_duplicates_count = "false";
            history_length = 20;
            horizontal_padding = 10;
            icon_position = "left";
            idle_threshold = 120;
            ignore_newline = "no";
            indicate_hidden = "yes";
            line_height = 0;
            markup = "full";
            max_icon_size = 32;
            monitor = 0;
            notification_height = 0;
            padding = 10;
            separator_color = "frame";
            separator_height = 2;
            show_age_threshold = 60;
            show_indicators = "yes";
            shrink = "no";
            sort = "yes";
            stack_duplicates = "true";
            startup_notification = "false";
            sticky_history = "yes";
            transparency = 0;
            verbosity = "mesg";
            word_wrap = "yes";
          };
          shortcuts = {
            close = "ctrl+space";
            close_all = "ctrl+shift+space";
            history = "ctrl+grave";
            context = "ctrl+shift+period";
          };
          urgency_low = {
            background = "#232323";
            foreground = "#A8A8A8";
            timeout = 3;
          };
          urgency_normal = {
            background = "#285577";
            foreground = "#ffffff";
            timeout = 5;
          };
          urgency_critical = {
            background = "#D64E4E";
            foreground = "#F0E0E0";
            frame_color = "#D64E4E";
            timeout = 7;
          };
        };
      };
      compton = {
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
      redshift = {
        enable = true;
        latitude = config.secrets.identity.redshiftLatitude;
        longitude = config.secrets.identity.redshiftLongitude;
        temperature.day = 5500;
        temperature.night = 3100;
        brightness.day = "1.0";
        brightness.night = "0.7";
        extraOptions = [ "-v" "-m randr" ];
      };
      udiskie = {
        enable = true;
        automount = true;
        notify = true;
        tray = "never";
      };
    };
    programs.gpg = {
      enable = true;
      settings = {
        keyserver = "hkp://keys.openpgp.org";
        require-cross-certification = true;
        use-agent = true;
      };
    };
    programs.ssh = {
      enable = true;
      forwardAgent = true;
      userKnownHostsFile = "~/.ssh/known_hosts";
      controlMaster = "auto";
      controlPath = "~/.ssh/sockets/%r@%h:%p";
      controlPersist = "4h";
      serverAliveInterval = 30;
    };
    programs.htop = {
      enable = true;
      fields = [ "USER" "PRIORITY" "NICE" "M_SIZE" "STATE" "PERCENT_CPU" "PERCENT_MEM" "TIME" "COMM" ];
      meters.left = [ "AllCPUs" "Memory" ];
      colorScheme = 0;
      detailedCpuTime = true;
    };
    programs.command-not-found.enable = true;
    programs.lesspipe.enable = true;
    programs.man.enable = true;
    programs.info.enable = true;
    programs.skim = {
      enable = true;
      historyWidgetOptions = [ "--exact" ];
      defaultOptions = [ "--height 40%" "--prompt ⟫" ];
      fileWidgetCommand = "${pkgs.fd}/bin/fd --type f";
      fileWidgetOptions = [ "--preview 'head {}'" ];
      changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type d";
      changeDirWidgetOptions = [ "--preview 'tree -C {} | head -200'" ];
      enableZshIntegration = true;
    };
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };
    programs.z-lua = {
      enable = true;
      enableZshIntegration = true;
      options = [ "fzf" "enhanced" "once" ];
    };
    programs.tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      escapeTime = 0;
      status = {
        currentWindowFormat = "#[bg=blue,fg=cyan,bold]#I#[bg=blue,fg=cyan]:#[fg=colour230]#T#[fg=dim]#F";
        windowFormat = "#[fg=cyan,dim]#I#[fg=blue]:#[default]#W#[fg=grey,dim]#F";
        leftFormat = "#{prefix_highlight}#[fg=green](#S) #(whoami)@#H";
        rightFormat = "#[fg=blue,bright]%k:%M:%S %d/%m/%Y | #{cpu_fg_color}#{cpu_icon}#{cpu_percentage}";
        style = "fg=white,bg=default,default";
        windowStyle = "fg=cyan,bg=default,dim";
        currentWindowStyle = "fg=colour166,bg=red,bright";
        messageStyle = "fg=white,bg=black,bright";
      };
      borderStyle = {
        active = "fg=colour240,bg=default";
        inactive = "fg=colour235,bg=default";
      };
      hooks = {
        "after-select-pane" =
          "run-shell \\\"tmux set -g window-active-style \"bg='brightblack'\" && sleep .05 && tmux set -g window-active-style ''\\\"";
      };
      bindings = {
        copyMode = {
          "M-e" = ''run-shell "${custom.shell-org-capture}/bin/shell-org-capture es"'';
          "M-j" = ''run-shell "${custom.shell-org-capture}/bin/shell-org-capture js"'';
          "M-n" = ''run-shell "${custom.shell-org-capture}/bin/shell-org-capture ns"'';
          "M-x" = ''run-shell "${custom.shell-org-capture}/bin/shell-org-capture xs"'';
        };
        root = {
          "C-left" = "prev";
          "C-right" = "next";
          "S-left" = "swap-window -t -1";
          "S-right" = "swap-window -t +1";
          "C-y" = ''
            run -b "exec </dev/null; ${pkgs.xsel}/bin/xsel -o --clipboard | tmux load-buffer - ; \
                                                tmux paste-buffer"'';
        };
        prefixed = {
          "*" = "list-clients";
          "l" = "refresh-client";
          "m" = "select-pane -m";
          "|" = ''split-window -h -c "#{pane_current_path}"'';
          "\\" = ''split-window -fh -c "#{pane_current_path}"'';
          "-" = ''split-window -v -c "#{pane_current_path}"'';
          "_" = ''split-window -fv -c "#{pane_current_path}"'';
          "'#'" = ''split-window -h -c "#{pane_current_path}"'';
          "@" = ''split-window -v -c "#{pane_current_path}"'';
          "BSpace" = "last-window";
          "r" = ''source-file ~/.tmux.conf \; display "  Config reloaded..."'';
          "y" = "set-window-option synchronize-panes";
          "T" = ''neww -n "Tmux manual" "exec man tmux"'';
          "s" = ''
            split-window -v "tmux list-sessions | sed -E 's/:.*$//' | \
                                                    grep -v \"^$(tmux display-message -p '#S')\$\" | \
                                                    ${pkgs.skim}/bin/sk --reverse | xargs tmux switch-client -t"'';
        };
      };
      extraConfig = ''
        set -g renumber-windows on

        set -g bell-action any
        set -g visual-activity off
        set -g visual-bell off
        set -g visual-silence off
        setw -g monitor-activity on
      '';
      historyLimit = 102400;
      keyMode = "emacs";
      nestedShortcut = "C-x";
      sensibleOnTop = false;
      shortcut = "M-x";
      terminal = "screen-256color";
      secureSocket = false;
      shell = "${pkgs.zsh}/bin/zsh";
      tmuxp.enable = true;
      plugins = with pkgs;
        with tmuxPlugins; [
          {
            plugin = fzf-tmux-url-with-history; # patched version, see overlays
            extraConfig = "set -g @fzf-url-bind 'o'";
          }
          battery
          copycat
          cpu
          fpp
          logging
          open # TODO: setup and verify working
          prefix-highlight
          sessionist
          yank
        ];
    };
    programs.alacritty = { # 75be5861a59b1eb04d33dbd38812a19f2665b9a0
      enable = true;
      settings = {
        env = { TERM = "xterm-256color"; };
        window = {
          padding = {
            x = 2;
            y = 2;
          };
          decorations = "full";
        };
        tabspaces = 8;
        draw_bold_text_with_bright_colors = true;
        visual_bell = {
          animation = "EaseOutExpo";
          duration = 1;
        };
        mouse_bindings = [{
          mouse = "Middle";
          action = "PasteSelection";
        }];
        selection = { semantic_escape_chars = '',│`|:"' ()[]{}<>''; };
        dynamic_title = true;
        cursor = { style = "Beam"; };
        live_config_reload = true;
      };
    };
    programs.bat = {
      enable = true;
      config = {
        theme = "TwoDark";
        # pager = "less -FR";
        pager = "${pkgs.most}/bin/most";
      };
    };
    programs.lsd = {
      enable = true;
      enableAliases = true;
    };
    programs.feh.enable = true;
    programs.zathura = {
      enable = true;
      options = {
        pages-per-row = 1;
      };
    };
    programs.zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        plugins = [ "colored-man-pages" "urltools" ];
        theme = "muse";
      };
      enableAutosuggestions = true;
      enableCompletion = true;
      history = {
        size = 10000;
        save = 10000;
        path = ".histfile";
        ignoreDups = true;
        expireDuplicatesFirst = true;
        extended = true;
        share = true;
      };
      initExtra = ''
        setopt APPEND_HISTORY
        setopt BRACE_CCL
        setopt HIST_FIND_NO_DUPS
        setopt HIST_IGNORE_ALL_DUPS
        setopt HIST_IGNORE_SPACE
        setopt HIST_NO_STORE
        setopt HIST_SAVE_NO_DUPS
        setopt AUTO_CD
        setopt EXTENDED_GLOB
        setopt INC_APPEND_HISTORY
        setopt MENU_COMPLETE

        ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin

        ${lib.concatMapStrings (opt: ''
          setopt ${opt}
        '') [
          "braceccl"
          "correctall"
          "extendedglob"
          "menucomplete"
        ]}

        bindkey '^P' fuzzy-search-and-edit
      '';
      sessionVariables = {
        HISTFILE = ".histfile";
        YSU_IGNORED_ALIASES = [ "g" "ll" ]; # TODO: review list
        YSU_MODE = "ALL";
        ZSH_COMMAND_TIME_COLOR = "cyan";
      };
      shellAliases = {
        cat = "${pkgs.bat}/bin/bat"; # use --plain in case of emergency

        less = "${pkgs.most}/bin/most";

        df = "${pkgs.dfc}/bin/dfc";
        du = "${pkgs.dua}/bin/dua";

        yg = "${pkgs.you-get}/bin/you-get";

        zz =
          "cd $(z -i | ${pkgs.skim}/bin/sk --nth 2 --reverse --inline-info --tac | ${pkgs.gawk}/bin/awk '{print $2}')";
        zb = "z -b";

        zr = ". ~/.zshrc";
      };
      plugins = [
        {
          name = "zsh-notify";
          file = "notify.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "marzocchi";
            repo = "zsh-notify";
            rev = "853bc9434771b99b028f069b95e13ecdf06901d0";
            sha256 = "0bhmv1xfjzmci9b4dy3mix2s31zj0kayrl44xx5xb8rgzlf0qbvr";
          };
        }
        {
          # TODO: try to integrate with fzf-based/skim utils, expecially commit browser
          name = "browse-commit";
          file = "browse-commit.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "wiedzmin";
            repo = "browse-commit";
            rev = "cf28b2eeba622545ae751ec99532b6b60e58b845";
            sha256 = "15c9qxxa7l47w5r28pazs0gv0016lv52mncn45s6g1d3120k5fx0";
          };
        }
        {
          name = "you-should-use";
          file = "you-should-use.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "MichaelAquilina";
            repo = "zsh-you-should-use";
            rev = "1.1.0";
            sha256 = "0fig5ralagi5jajk7gdm52jvwql17qk9cd6j98qsndvckb26a753";
          };
        }
        {
          name = "pass-zsh-completion";
          file = "pass-zsh-completion.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "ninrod";
            repo = "pass-zsh-completion";
            rev = "e4d8d2c27d8999307e8f34bf81b2e15df4b76177";
            sha256 = "1z83hgdljl7yqd1lqb10an8zkrv7s01khky27mgc1wargkslkxi9";
          };
        }
        {
          name = "zsh-async";
          file = "async.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "mafredri";
            repo = "zsh-async";
            rev = "e6d937228729f934f2033039bb54c3a18f5f1358";
            sha256 = "0f0bqm4245ghx31x30ircfp4njji834495g25wvrd93k2r96a669";
          };
        }
        {
          name = "git-extra-commands";
          file = "git-extra-commands.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "unixorn";
            repo = "git-extra-commands";
            rev = "f03ff8ffce9f3e488b6a0265cb09288cc29899fe";
            sha256 = "1qlbjn0q87jgjir3k7w4m8p6wqgjl2c7jnilczf7c205fgwksdhi";
          };
        }
        {
          name = "zsh-reentry-hook";
          file = "zsh-reentry-hook.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "RobSis";
            repo = "zsh-reentry-hook";
            rev = "8587186df8f08b8a57ae7f87ab0bc7d503909031";
            sha256 = "1jgin1gmw05vxf7vw414zvhq9dg06yzlzxas723f710vs58mf11a";
          };
        }
        {
          name = "zsh-fuzzy-search-and-edit";
          file = "plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "seletskiy";
            repo = "zsh-fuzzy-search-and-edit";
            rev = "4fbb3d351b75f1007df0d5cb09292bb2321f903a";
            sha256 = "1shhmda1iqwz79y2ianmjs5623zabckxfj2hqw4gl2axpkwnj1ib";
          };
        }
        {
          name = "zsh-command-time";
          file = "command-time.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "popstas";
            repo = "zsh-command-time";
            rev = "afb4a4c9ae7ce64ca9d4f334a79a25e46daad0aa";
            sha256 = "1bvyjgz6bhgg1nwr56r50p6fblgah6yiql55pgm5abnn2h876fjq";
          };
        }
        {
          # NOTE: should be last in the list
          name = "zsh-syntax-highlighting";
          file = "zsh-syntax-highlighting.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-syntax-highlighting";
            rev = "e900ad8bad53501689afcb050456400d7a8466e5";
            sha256 = "1dfy5wvkmnp2zzk81fhc7qlywgn0j6z0vjch5ak5r3j2kqv61cmi";
          };
        }
      ];
    };
    home.stateVersion = "19.09";
  };
}

# TODO: some impl/binding for ix.io posts
# https://github.com/embayer/org-note
