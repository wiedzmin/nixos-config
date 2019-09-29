{ config, pkgs, lib, ... }:
with import <home-manager/modules/lib/dag.nix> { inherit lib; };
with import ./const.nix { inherit config pkgs; };
with import ./secrets/const.nix { inherit config pkgs lib; };
let
  custom = import ../../pkgs/custom pkgs config;
  userCustom = import ./custom pkgs config;
in {
  imports = [ <home-manager/nixos> ./config ./packages.nix ./secrets/personal.nix ./secrets/job.nix ];

  attributes.mainUser = "${userName}";

  users.extraUsers."${userName}" = {
    isNormalUser = true;
    uid = 1000;
    description = "Alex Ermolov";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ];
  };

  services.batteryNotifier = {
    enable = true;
    notifyCapacity = 20;
    suspendCapacity = 10;
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
    xkeysnail = {
      enable = true;
      configFile = "/home/${userName}/.config/xkeysnail/config.py";
    };
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

  screenshots = {
    enable = true;
    baseDir = "/home/${userName}/screenshots";
    dateFormat = "+%Y-%m-%d_%H:%M:%S";
    calendarTimespec = "*-*-* 00:05:00";
  };

  services.xsuspender.enable = true;

  services.git-fetch-updates = {
    enable = true;
    workDir = "/home/${userName}";
    bootTimespec = "1min";
    activeTimespec = "30min";
  };

  services.git-push-updates = {
    enable = false;
    calendarTimespec = "*-*-* 18:00:00";
  };

  services.git-save-wip = {
    enable = false;
    bootTimespec = "30sec";
    activeTimespec = "1hour";
  };

  services.clean-trash = {
    enable = true;
    calendarTimespec = "*-*-* 23:00:00";
  };

  services.xidlehook.enable = true;

  # FIXME: move functionality to pkgsctl
  system.activationScripts.saveCurrentHMVersion = ''
    touch /etc/current-home-manager
    cd /etc/nixos/pkgs/forges/github.com/rycee/home-manager
    hm_revision="upstream: $(${pkgs.git}/bin/git rev-parse --short HEAD)"
    echo "$hm_revision" >> /etc/current-home-manager
  '';

  nix.trustedUsers = [ userName ];

  networking.extraHosts = (builtins.concatStringsSep "\n"
    (map (host: host.ip + "   " + (builtins.concatStringsSep " " host.hostNames)) (jobExtraHosts ++ extraHosts)));

  home-manager.users."${userName}" = {
    nixpkgs.config.allowUnfree = true;
    xdg.enable = true;
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
        musicDirectory = "/home/${userName}/blobs/music";
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
