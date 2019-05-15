{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        gotop
        htop
        iotop
        lsof
        psmisc
    ];
}
