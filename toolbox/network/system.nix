{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        afpfs-ng
        networkmanager
        networkmanagerapplet
        ssh-agents
        wirelesstools
    ];
}
