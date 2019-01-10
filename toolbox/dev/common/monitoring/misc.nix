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
        eventstat
        inotify-tools
        multitail
        ntfy
        pv
        reflex
        up
        watchexec
    ];
}
