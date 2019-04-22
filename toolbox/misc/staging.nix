{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        cdimgtools
        libwhich
        semver-tool
        sit
        websocat
        xtruss
        zzuf
    ];
}
