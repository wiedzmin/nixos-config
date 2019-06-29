{ config, pkgs, lib, ... }:

let
    enableScanner = false;
in
{
    imports = [
        ../pkgs/setup.nix
        ../partitions/laptoptop-ssd-512.nix
        ../users/alex3rd
        <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

    nix = { # per-machine settings
        maxJobs = lib.mkDefault 4;
        buildCores = lib.mkDefault 4;
        optimise.automatic = true;
        gc = {
            automatic = true;
            dates = "weekly";
            options = "--delete-older-than 7d";
        };
    };

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

    system.activationScripts.ensureBacklightPermissions = ''
        chmod a+w /sys/class/backlight/intel_backlight/brightness
    '';

    hardware = {
        bluetooth = {
            enable = true;
            powerOnBoot = false;
        };
        cpu.intel.updateMicrocode = true;
        enableAllFirmware = true;
        enableKSM = true;
        opengl = {
            enable = true;
            extraPackages = with pkgs; [
                intel-media-driver
                libvdpau-va-gl
                vaapiIntel
                vaapiVdpau
            ];
            driSupport32Bit = true;
            extraPackages32 = with pkgs.pkgsi686Linux; [
                libvdpau-va-gl
                vaapiIntel
                vaapiVdpau
            ];
        };
        trackpoint = {
            enable = true;
            sensitivity = 255;
            speed = 200;
            emulateWheel = true;
        };
        pulseaudio = {
            enable = true;
            support32Bit = true;
            package = pkgs.pulseaudioFull; # 'full' for e.g. bluetooth
            systemWide = true;
            daemon.config = {
                flat-volumes = "no";
            };
        };
        sane = {
            enable = enableScanner;
            extraBackends = lib.mkIf enableScanner [ pkgs.epkowa ];
        };
    };

    boot = {
        loader.grub = {
            enable = true;
            version = 2;
            device = "/dev/sda";
            configurationLimit = 10;
        };
        vesa = true;
        initrd.availableKernelModules = [
            "ahci"
            "ehci_pci"
            "sdhci_pci"
            "usb_storage"
            "xhci_pci"
        ];
        plymouth.enable = true;
        extraModprobeConfig = ''
            options iwlwifi 11n_disable=1 power_save=1 power_level=2
            options kvm-intel nested=1
        '';
        extraModulePackages = with config.boot.kernelPackages; [
            exfat-nofuse
        ];
        tmpOnTmpfs = true;
        kernelPackages = pkgs.linuxPackages_4_19;
        kernelParams = [
            "scsi_mod.use_blk_mq=1"
            "pti=off"
            "nospectre_v1"
            "nospectre_v2"
            "l1tf=off"
            "nospec_store_bypass_disable"
        ];
        kernelModules = [
            "bfq"
            "kvm-intel"
            "thinkpad_acpi"
            "thinkpad_hwmon"
        ];
        kernel.sysctl = {
            "fs.inotify.max_user_instances" = 1024;
            "fs.inotify.max_user_watches" = 1048576;
            "fs.inotify.max_queued_events" = 32768;
            "net.ipv4.ip_forward" = 1; # for VMs forwarding
            "net.ipv4.ip_default_ttl" = 65;
            "net.ipv4.tcp_sack" = 0;
        };
    };

    environment.systemPackages = with pkgs; with config.boot.kernelPackages; [
        pasystray
        pavucontrol
        # ocz-ssd-guru # add as an overlay and fix hash (and installation instructions)
        intelmetool
        me_cleaner
    ] ++ [
        perf
        cpupower
        hotspot
    ];
    powerManagement = {
        enable = true;
        powertop.enable = true;
    };

    services = {
        irqbalance.enable = true;
        ympd = {
            enable = true;
            webPort = 8765;
        };
        chrony.enable = true;
        openssh = {
            enable = true;
            allowSFTP = true;
            forwardX11 = false;
        };
        smartd = {
            enable = true;
            notifications = {
                x11.enable = true;
            };
        };
        journald.extraConfig = ''
            MaxRetentionSec=7day
        '';
        udev.extraRules = ''
            ACTION=="add|change", KERNEL=="sd[ab][!0-9]", ATTR{queue/scheduler}="kyber"
        '';
        upower.enable = true;
        tlp.enable = true;
        acpid.enable = true;
        printing = {
            enable = false;
            drivers = [ pkgs.hplip ];
            browsing = true;
            defaultShared = true;
        };
    };

    users.extraUsers.root.hashedPassword = "586c56b7b6b6f68fca29c9ff2524e4dc52d51d5b6184a65f707dd3eae075e4c9afa81c9cd4042c26c9fb773d4f3de55fb55f363c6b0f5f6790baf4c4e3f32cb9";
    nix.trustedUsers = [ "root" ];
    security = {
        sudo.wheelNeedsPassword = false;
        polkit.extraConfig = ''
            /* Allow users in wheel group to manage systemd units without authentication */
            polkit.addRule(function(action, subject) {
                if (action.id == "org.freedesktop.systemd1.manage-units" &&
                    subject.isInGroup("wheel")) {
                    return polkit.Result.YES;
                }
            });
        '';
        wrappers = {
            pmount.source = "${pkgs.pmount}/bin/pmount";
            pumount.source = "${pkgs.pmount}/bin/pumount";
            sudo = {
                source = "${pkgs.sudo}/bin/sudo";
                owner = "root";
                permissions = "u+s";
            };
        };
        allowUserNamespaces = true;
        allowSimultaneousMultithreading = true;
        lockKernelModules = false;
    };

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

    nixpkgs.config = {
        allowUnfree = true;
        allowUnfreeRedistributable = true;
        sane.snapscanFirmware = lib.mkIf enableScanner "/etc/nixos/contrib/blobs/Esfw52.bin";
    };

    networking = {
        hostName = "laptoptop";
        hostId = "2ab69157";
        firewall.enable = false;
        usePredictableInterfaceNames = lib.mkForce true;
        dnsExtensionMechanism = false;
        wlanInterfaces = {
            "wlan0" = { device = "wlp3s0"; };
        };
        networkmanager = {
            enable = true;
            unmanaged = [
                "interface-name:ve-*"
            ];
        };
    };

    system.stateVersion = "19.03";
}
