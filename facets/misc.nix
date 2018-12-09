{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        android-file-transfer
        appimage-run
        datefudge
        deskew
        fpart
        homebank
        hyperfine
        jdupes
        skrooge
        super
        inkscape
    ];
}
