{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        appimage-run
        gcalcli
        # gitAndTools.git-annex # FIXME: try to overlay
        # gitAndTools.git-annex-remote-rclone
        wirelesstools
        lsof # TODO: think of moving appropriately
    ];
}
