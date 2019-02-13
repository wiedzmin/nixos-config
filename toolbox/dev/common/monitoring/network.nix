{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        gping
        jnettop
        nethogs
        nload
        speedtest-cli
    ];
}
