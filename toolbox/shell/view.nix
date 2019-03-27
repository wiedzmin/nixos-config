{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        bat
        bcat
        fpp
        glogg
        pspg
        tealdeer
    ];
}
