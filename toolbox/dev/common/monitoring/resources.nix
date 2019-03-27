{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        duc
        gotop
        htop
        iotop
        lsof
        psmisc
    ];
}
