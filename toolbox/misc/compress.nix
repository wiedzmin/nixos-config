{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        archiver # TODO: add to shell aliases or like
        cabextract
        unshield
    ];
}
