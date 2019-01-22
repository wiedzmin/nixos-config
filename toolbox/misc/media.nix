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
        mimeo
        mpv
        playerctl
        squashfsTools
        vlc
        xorriso
    ];
}
