{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # resources
        diskus
        duc
        gotop
        htop
        iotop
        lsof
        psmisc
        smem

        # misc
        # ocz-ssd-guru # add as an overlay and fix hash (and installation instructions)
        inotify-tools
        multitail
        pscircle
        pv
        up
    ];
}
