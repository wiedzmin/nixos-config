{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        appimage-run
        gcalcli
        gitAndTools.git-annex
        gitAndTools.git-annex-remote-rclone
        wirelesstools
        lsof # TODO: think of moving appropriately
    ];
}
