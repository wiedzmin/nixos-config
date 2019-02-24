{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        homebank
    ];
}
