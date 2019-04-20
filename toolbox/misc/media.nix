{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        android-file-transfer
        cdrkit
        exif
        ffmpeg
        geteltorito
        gimp
        maim
        mimeo
        mpv
        playerctl
        python3Packages.mps-youtube
        squashfsTools
        vlc
        xorriso
    ];
}
