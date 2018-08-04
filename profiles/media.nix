{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        exif
        ffmpeg
        gimp
        mpv
    ];
}
