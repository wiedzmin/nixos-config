{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        android-file-transfer
        exif
        ffmpeg
        gimp
        inkscape
        maim
        mpv
        playerctl
    ];
}
