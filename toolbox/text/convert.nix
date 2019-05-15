{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        ditaa
        gron
        ntangle
        pandoc
        pdftk
        plantuml
    ];
}
