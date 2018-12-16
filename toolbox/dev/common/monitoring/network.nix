{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        iperf
        nethogs
        nload
        speedtest-cli
    ];
}
