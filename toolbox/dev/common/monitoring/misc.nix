{ config, pkgs, ... }:

{
    programs = {
        mtr.enable = true;
        wireshark = {
            enable = true;
            package = pkgs.wireshark-gtk;
        };
        wavemon.enable = true;
    };

    environment.systemPackages = with pkgs; [
        inotify-tools
        lnav
        ntfy
        pv
        reflex
        up
        watchexec
    ];
}
