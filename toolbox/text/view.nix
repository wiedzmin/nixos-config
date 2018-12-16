{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        calibre
        djview
        djvulibre
        ghostscript
        polar-bookshelf
        zathura
    ];
}
