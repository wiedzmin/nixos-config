{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        miniserve
        socat
        websocat
    ];
}
