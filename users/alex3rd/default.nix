{ config, pkgs, lib, ... }:
with import <home-manager/modules/lib/dag.nix> { inherit lib; };
with import ./secrets/const.nix { inherit config pkgs lib; };
let
  custom = import ../../pkgs/custom pkgs config;
  userCustom = import ./custom pkgs config;
in {
  imports = [ <home-manager/nixos> ./config ./packages.nix ./secrets/personal.nix ./secrets/job.nix ];

  attributes.mainUser = {
    name = "alex3rd";
    fullName = "${userFullName}";
    email = "${userEmail}";
    gpgKeyID = "${userPrimaryGpgKeyID}";
  };

  users.extraUsers."${config.attributes.mainUser.name}" = {
    isNormalUser = true;
    uid = 1000;
    description = "${userFullName}";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ];
  };

  attributes.testLines = lib.mkAfter ''

    333
    444
  '';

  services.batteryNotifier = {
    enable = true;
    notifyCapacity = 20;
    suspendCapacity = 10;
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

  media = {
    enable = true;
    pulse = {
        enable = true;
        daemonConfig = {
          flat-volumes = "no";
        };
    };
    opengl.enable = true;
    ympd.enable = true;
  };

  virtualization = {
    enable = true;
    docker = {
      enable = true;
      devdns.enable = true;
    };
    libvirt.enable = true;
  };

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

  appearance = {
    enable = true;
    fonts = {
      antialias = true;
      list = with pkgs; [
        anonymousPro
        corefonts
        emacs-all-the-icons-fonts
        fantasque-sans-mono
        fira-code
        font-awesome-ttf
        go-font
        hack-font
        iosevka
        powerline-fonts
        terminus_font
        terminus_font_ttf
      ];
      console = "Lat2-Terminus16";
    };
    wallpaper = {
      enable = true;
      root = "/home/${config.attributes.mainUser.name}/blobs/wallpaper";
      current = "mongolia_2.jpg";
    };
  };

  screenshots = {
    enable = true;
    baseDir = "/home/${config.attributes.mainUser.name}/screenshots";
    dateFormat = "+%Y-%m-%d_%H:%M:%S";
    calendarTimespec = "*-*-* 00:05:00";
  };

  services.xsuspender.enable = true;

  services.clean-trash = {
    enable = true;
    calendarTimespec = "*-*-* 23:00:00";
  };

  dev.python.enable = true;

  email = {
    enable = true;
    emailAddress = userEmail;
    passwordPath = userGoogleAccountPasswordPath;
    gpg = {
      sign = true;
      keyID = userPrimaryGpgKeyID;
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
      byFrom = imapfilterFromToFolder;
      byTo = imapfilterToToFolder;
      byCc = imapfilterCcToFolder;
      bySubject = imapfilterSubjectToFolder;
      deleteByFrom = imapfilterFromDelete;
    };
  };

  services.xidlehook.enable = true;

  themes.condensedFonts.enable = true;
  themes.zenburn.enable = true;

  # FIXME: move functionality to pkgsctl
  system.activationScripts.saveCurrentHMVersion = ''
    touch /etc/current-home-manager
    cd /etc/nixos/pkgs/forges/github.com/rycee/home-manager
    hm_revision="upstream: $(${pkgs.git}/bin/git rev-parse --short HEAD)"
    echo "$hm_revision" >> /etc/current-home-manager
  '';

  nix.trustedUsers = [ config.attributes.mainUser.name ];

  networking.extraHosts = (builtins.concatStringsSep "\n"
    (lib.mapAttrsToList (ip: hosts: ip + "    " + (builtins.concatStringsSep " " hosts)) (jobExtraHosts // extraHosts)));

  home-manager.users."${config.attributes.mainUser.name}" = {
    nixpkgs.config.allowUnfree = true;
    xdg.enable = true;
    home.file = {
      "test_lines".text = config.attributes.testLines;
    };
    home.packages = with pkgs; [
      # custom.gen-nix-du
      custom.confctl
      custom.format-config
      custom.pkgsctl
      custom.update-system
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
    };
    programs.gpg = {
      enable = true;
      settings = {
        keyserver = "hkp://keys.openpgp.org";
        require-cross-certification = true;
        use-agent = true;
      };
    };
    home.stateVersion = "19.09";
  };
}
