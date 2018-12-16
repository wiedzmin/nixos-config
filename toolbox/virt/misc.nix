{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        appimage-run
    ];
}
