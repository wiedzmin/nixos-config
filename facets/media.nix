{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        exif
        ffmpeg
        gimp
        maim
        mpv
        youtube-dl
    ];
}
