{ config, pkgs, ... }:

{
    services.mpd.enable = true;

    environment.systemPackages = with pkgs; [
        appimage-run
        dateutils
        dex
        duc
        gcalcli
        gotop
        homebank
        inotify-tools
        lsof
        plan9port
        pscircle
        wirelesstools
        youtube-dl
        tealdeer
        # ocz-ssd-guru # add as an overlay and fix hash (and installation instructions)
    ];
}
