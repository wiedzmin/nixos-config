{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # resources
        duc
        gotop
        htop
        iotop
        lsof
        psmisc

        # misc
        # ocz-ssd-guru # add as an overlay and fix hash (and installation instructions)
        inotify-tools
        multitail
        pscircle
        pv
    ];
}
