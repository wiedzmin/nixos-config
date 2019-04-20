{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        apktool
        archiver # TODO: add to shell aliases or like
        cabextract
        gnutar
        lzip
        p7zip
        unrar
        unshield
        unzip
        zip
    ];
}
