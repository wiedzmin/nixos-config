{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        qbittorrent
        skype
        slack
        tdesktop
        teamviewer
        traceroute
        w3m
        you-get
        youtube-dl
        zoom-us
    ];
}
