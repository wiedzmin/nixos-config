{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        archiver # TODO: add to shell aliases or like
        cabextract
        gnutar
        p7zip
        unrar
        unshield
        unzip
        zip
    ];
}
