{ config, pkgs, ... }:

{
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
        wavemon.enable = true;
    };

    environment.systemPackages = with pkgs; [
        # TODO: maybe split even further
        afpfs-ng
        chromium
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
        rclone-browser
        skype
        slack
        socat
        speedtest-cli
        tdesktop
        tdlib
        telega-server
        telegram-cli
        w3m
        wget
        wirelesstools
    ];
}
