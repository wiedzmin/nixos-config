{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        appimage-run
        gitAndTools.git-annex
        gitAndTools.git-annex-remote-rclone
    ];
}
