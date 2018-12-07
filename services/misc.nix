{ config, pkgs, ... }:
{
    services = {
        irqbalance.enable = true;
        mpd.enable = true;
        chrony.enable = true;
    };
}
