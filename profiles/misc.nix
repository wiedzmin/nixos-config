{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        appimage-run
        gcalcli
        gitAndTools.git-annex
        gitAndTools.git-annex-remote-rclone
        wirelesstools
        nrg2iso
        lsof # TODO: think of moving appropriately
    ];
}
