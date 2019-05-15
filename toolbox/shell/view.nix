{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        fpp
        glogg
    ];
}
