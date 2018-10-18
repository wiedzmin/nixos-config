{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        appimage-run
        dateutils
        dex
        duc
        gcalcli
        gitAndTools.git-annex
        gitAndTools.git-annex-remote-rclone
        lsof
        plan9port
        pscircle
        whipper
        wirelesstools
        youtube-dl
    ];
}
