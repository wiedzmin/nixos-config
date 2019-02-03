{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        iperf
        jnettop
        nethogs
        nload
        speedtest-cli
    ];
}
