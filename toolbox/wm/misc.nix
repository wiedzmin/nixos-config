{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        xsuspender
    ];
}
