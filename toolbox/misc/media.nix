{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        (mpv-with-scripts.override ({
            scripts = [ mpvScripts.mpris ];
        }))
        android-file-transfer
        cdrkit
        exif
        ffmpeg
        geteltorito
        gimp
        maim
        mimeo
        playerctl
        python3Packages.mps-youtube
        squashfsTools
        vlc
        xorriso
    ];
}
