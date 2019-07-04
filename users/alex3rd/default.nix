{config, pkgs, lib, ...}:
with import <home-manager/modules/lib/dag.nix> { inherit lib; };
with import ./const.nix {inherit config pkgs;};
with import ./private/sshuttle.nix {inherit config pkgs lib;};
{
    imports = [
        <home-manager/nixos>
        ./config
        ./modules
        ./packages.nix
        ../../pkgs/scripts
        ./private/network.nix
    ];

    users.extraUsers."${userName}" = {
        isNormalUser = true;
        uid = 1000;
        description = "Alex Ermolov";
        shell = pkgs.zsh;
        extraGroups = [
            "audio"
            "input"
            "lp"
            "networkmanager"
            "scanner"
            "video"
            "wheel"
        ];
    };

    services.batteryNotifier = {
        enable = true;
        notifyCapacity = 20;
        suspendCapacity = 10;
    };

    services.fusuma.enable = true;

    services.order-screenshots = {
        enable = true;
        baseDir = "/home/${userName}/screenshots";
        calendarTimespec = "*-*-* 00:05:00";
    };

    services.sshuttle = {
        enable = true;
        remote = sshuttleRemote;
        excludeSubnets = sshuttleExcludes;
        sshIdentity = sshuttleIdentity;
    };

    services.xkeysnail = {
        enable = true;
        configFile = "/home/${userName}/.config/xkeysnail/config.py";
    };

    services.vpn-client = {
        enable = true;
        name = "jobvpn";   # TODO: templatize
        keep = true;
        configPath = "/etc/nixos/users/${userName}/private/vpn/job.current/office.ovpn";
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

    system.activationScripts.saveCurrentHMVersion = ''
        cd /etc/nixos/pkgs/home-manager
        hm_revision=$(${pkgs.git}/bin/git rev-parse --short HEAD)
        echo "$hm_revision" > /etc/current-home-manager
    '';

    nix.trustedUsers = [ userName ];

    networking.extraHosts = (builtins.concatStringsSep "\n"
                                      (map (host: host.ip + "   " + (builtins.concatStringsSep " " host.hostNames))
                                      (config.job.extraHosts ++ config.misc.extraHosts)));

    home-manager.users."${userName}" = {
        nixpkgs.config.allowUnfree = true;
        xdg.enable = true;
        home.packages = with pkgs; [
            # base
            file
            glibcLocales
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
