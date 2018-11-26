{ config, pkgs, ... }:
{
    services = {
        irqbalance.enable = true;
    };
}
