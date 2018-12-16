{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        diskus
        duc
        gotop
        htop
        iotop
        lsof
        pscircle
        psmisc
        smem
    ];
}
