{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        duc
        gotop
        htop
        iotop
        lsof
        linuxPackages_4_14.perf # TODO: make SPOT (refer to machines)
        psmisc
    ];
}
