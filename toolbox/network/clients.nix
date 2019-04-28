{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        chromium
        lynx
        qbittorrent
        qutebrowser
        skype
        slack
        tdesktop
        teamviewer
        telegram-cli
        traceroute
        w3m
        wget
        you-get
        youtube-dl
        zoom-us
    ];
}
