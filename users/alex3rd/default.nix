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
        ../scripts.nix
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

    services.fusuma = {
        enable = true;
        userName = "alex3rd";   # TODO: templatize
    };

    services.order-screenshots = {
        enable = true;
        baseDir = "/home/alex3rd/screenshots";    # TODO: templatize
    };

    services.sshuttle = {
        enable = true;
        remote = sshuttleRemote;
        excludeSubnets = sshuttleExcludes;
        sshIdentity = sshuttleIdentity;
    };

    services.xkeysnail = {
        enable = true;
        userName = "alex3rd";   # TODO: templatize
    };

    services.openvpn = {
        servers = {
            jobvpn = {
                # TODO: make more declarative, i.e. to hide private part and automate all the rest
                config = ''config /etc/nixos/users/${userName}/private/vpn/job.current/office.ovpn'';
                autoStart = false;
                up = "${pkgs.update-resolv-conf}/libexec/openvpn/update-resolv-conf";
                down = "${pkgs.update-resolv-conf}/libexec/openvpn/update-resolv-conf";
            };
        };
    };

    services.keep-vpn = {
        enable = true;
        vpnName = "jobvpn";   # TODO: templatize
    };

    services.xsuspender = {
        enable = true;
        userName = "alex3rd";   # TODO: templatize
    };

    services.git-fetch-updates = {
        enable = true;
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
        home.packages = with pkgs; [
            # base
            file
            glibcLocales
        ];
        services.gpg-agent = {
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
        programs.gpg = {
            enable = true;
            settings = {
                keyserver = "hkp://keys.gnupg.net";
                require-cross-certification = true;
                use-agent = true;
            };
        };
        home.stateVersion = "19.09";
    };
}
