{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        (mpv-with-scripts.override ({
            scripts = [ mpvScripts.mpris ];
        }))
        android-file-transfer
        aria2
        exif
        ffmpeg
        gimp
        maim
        mimeo
        playerctl
        python3Packages.mps-youtube
        minitube
    ];
}
