{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        calibre
        djview
        djvulibre
        ghostscript
        zathura
    ];
}
