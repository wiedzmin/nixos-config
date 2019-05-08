{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        pazi
        amber
        cdimgtools
        extrace
        jump
        libwhich
        pforth
        rmount
        semver-tool
        sit
        websocat
        xtruss
        z-lua
        zzuf
    ];
}
