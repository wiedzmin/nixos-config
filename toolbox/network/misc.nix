{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        miniserve
        netcat
        rclone
        rclone-browser
        socat
    ];
}
