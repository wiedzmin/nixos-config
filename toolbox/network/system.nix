{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        afpfs-ng
        ssh-agents
        wirelesstools
    ];
}
