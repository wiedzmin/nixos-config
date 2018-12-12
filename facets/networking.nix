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
        http-prompt
        httpie
        iperf
        lynx
        miniserve
        mosh
        netcat
        nethogs
        netsniff-ng
        networkmanager
        networkmanager_dmenu
        networkmanagerapplet
        ngrep
        nload
        pcapfix
        qbittorrent
        rclone
        rclone-browser
        skype
        slack
        socat
        speedtest-cli
        ssh-agents
        tdesktop
        tdlib
        telega-server
        telegram-cli
        w3m
        wget
        wirelesstools
        you-get
        zoom-us
    ];
}
