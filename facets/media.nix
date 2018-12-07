{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        exif
        ffmpeg
        gimp
        maim
        mpv
        perlPackages.ImageExifTool
        # rapid-photo-downloader
        youtube-dl
    ];
}
