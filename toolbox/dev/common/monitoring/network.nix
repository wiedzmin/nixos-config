{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        jnettop
        nethogs
        nload
        speedtest-cli
    ];
}
