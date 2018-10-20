{ config, pkgs, ... }:

{
    nixpkgs.config.firefox = {
        enableAdobeFlashDRM = true;
        enableDjvu = true;
        enableGnomeExtensions = true;
        jre = false;
        icedtea = true;
    };

    services = {
        psd = {
            enable = true;
            resyncTimer = "30min";
        };
        openssh = {
            enable = true;
            forwardX11 = true;
        };
    };

    programs = {
        mtr.enable = true;
        wireshark = {
            enable = true;
            package = pkgs.wireshark-gtk;
        };
    };

    environment.systemPackages = with pkgs; [
        # TODO: maybe split even further
        afpfs-ng
        chromium
        firefox
        iperf
        lynx
        mosh
        netcat
        nethogs
        netsniff-ng
        networkmanager
        networkmanager_dmenu
        networkmanagerapplet
        nload
        pcapfix
        qbittorrent
        rclone
        skype
        slack
        socat
        tdesktop
        tdlib
        telega-server
        telegram-cli
        w3m
        wget
        youtube-dl
    ];
}
