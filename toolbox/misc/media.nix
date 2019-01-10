{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        android-file-transfer
        cdrkit
        exif
        ffmpeg
        geteltorito
        gimp
        handbrake
        inkscape
        maim
        mpv
        playerctl
        squashfsTools
        xorriso
    ];
}
