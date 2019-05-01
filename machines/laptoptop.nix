{ config, pkgs, lib, ... }:

{
    imports = [
        ../pkgs/setup.nix
        ../partitions/laptoptop-ssd-512.nix
        ../hardware/bluetooth.nix
        ../hardware/intel.nix
        ../hardware/laptop.nix
        ../hardware/misc.nix
        ../hardware/sound.nix
        ../users/alex3rd/services/xserver.nix
        ../users/alex3rd/default.nix
        <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

    boot.loader.grub = {
        enable = true;
        version = 2;
        device = "/dev/sda";
        configurationLimit = 10;
    };
    boot.plymouth.enable = true;
    boot.tmpOnTmpfs = true;

    boot.kernelPackages = pkgs.linuxPackages_custom {
        version = "4.14.114";
        configfile = ../overlay/kernel.config;
        src = pkgs.fetchurl {
            url = "https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.14.114.tar.xz";
            sha256 = "05cyq4id1l3z8hhfs7ril9qc92pfx9h9pgvwl18q1lf8mg7ispmp";
        };
    };

    services = {
        irqbalance.enable = true;
        mpd.enable = true;
        ympd.enable = true;
        chrony.enable = true;
        openssh = {
            enable = true;
            forwardX11 = true;
        };
        syncthing = {
            enable = true;
        };
    };

    system.activationScripts.ensureSyncthingDataSymlinks = ''
        mkdir -p ${config.services.syncthing.dataDir}/bookshelf
        chown -R ${config.services.syncthing.user}:${config.services.syncthing.group} ${config.services.syncthing.dataDir}/bookshelf
    '';

    security.sudo.wheelNeedsPassword = false;

    users.extraUsers.root.hashedPassword = "586c56b7b6b6f68fca29c9ff2524e4dc52d51d5b6184a65f707dd3eae075e4c9afa81c9cd4042c26c9fb773d4f3de55fb55f363c6b0f5f6790baf4c4e3f32cb9";
    nix.trustedUsers = [ "root" ];

    security.polkit.extraConfig = ''
        /* Allow users in wheel group to manage systemd units without authentication */
        polkit.addRule(function(action, subject) {
            if (action.id == "org.freedesktop.systemd1.manage-units" &&
                subject.isInGroup("wheel")) {
                return polkit.Result.YES;
            }
        });
    '';

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
        enable = false;
        drivers = [ pkgs.hplip ];
    };

    # scanner
    # nixpkgs.config = {
    #     sane.snapscanFirmware = "/etc/nixos/contrib/blobs/Esfw52.bin";
    # };

    hardware.sane = {
        enable = false;
        extraBackends = [ pkgs.epkowa ];
    };

    nixpkgs.config.allowUnfree = true;

    networking = {
        hostName = "laptoptop";
        hostId = "2ab69157";
        firewall.enable = false;
        usePredictableInterfaceNames = true;
        wlanInterfaces = {
            "wlan0" = { device = "wlp3s0"; };
        };
        networkmanager = {
            enable = true;
            unmanaged = [ "interface-name:ve-*" ];
        };
    };

    system.stateVersion = "19.03";
}
