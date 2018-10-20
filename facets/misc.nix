{ config, pkgs, ... }:

{
    services.mpd.enable = true;

    environment.systemPackages = with pkgs; [
        appimage-run
        dateutils
        dex
        duc
        gcalcli
        gitAndTools.git-annex
        gitAndTools.git-annex-remote-rclone
        gotop
        homebank
        inotify-tools
        lsof
        plan9port
        pscircle
        whipper
        wirelesstools
        youtube-dl
    ];
}
