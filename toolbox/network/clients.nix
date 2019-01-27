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
        telegram-cli
        w3m
        wget
        you-get
        youtube-dl
        zoom-us
    ];
}
