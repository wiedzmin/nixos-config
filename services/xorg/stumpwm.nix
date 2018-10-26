{ config, pkgs, ... }:
{
    services.xserver.windowManager = {
        default = "stumpwm";
        stumpwm.enable = true;
    };
}
