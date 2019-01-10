{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
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
