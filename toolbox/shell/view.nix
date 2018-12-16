{ config, pkgs, ... }:

{
    services.locate.enable = true;

    environment.systemPackages = with pkgs; [
        bat
        bcat
        fpp
        glogg
        tealdeer
    ];
}
