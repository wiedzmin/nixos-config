{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        cdimgtools
        libwhich
        semver-tool
        websocat
        xtruss
        zzuf
    ];
}
