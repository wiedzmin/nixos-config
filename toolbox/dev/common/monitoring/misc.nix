{ config, pkgs, ... }:

{
    programs = {
        mtr.enable = true;
        wireshark = {
            enable = true;
            package = pkgs.wireshark-qt;
        };
        wavemon.enable = true;
    };

    environment.systemPackages = with pkgs; [
        ntfy
        pv
        reflex
        up
        watchexec
    ];
}
