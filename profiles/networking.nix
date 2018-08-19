{ config, pkgs, ... }:

{
    nixpkgs.config.firefox = {
        enableAdobeFlashDRM = true;
        enableDjvu = true;
        enableGnomeExtensions = true;
        jre = false;
        icedtea = true;
    };
    nixpkgs.config.chromium = {
        enablePepperPDF = true;
    };

    environment.systemPackages = with pkgs; [
        # TODO: maybe split even further
        chromium
        firefox
        iperf
        lynx
        mosh
        netcat
        nethogs
        networkmanagerapplet
        qbittorrent
        rclone
        skype
        slack
        socat
        tdesktop
        telegram-cli
        w3m
        wget
        youtube-dl
    ];
}
