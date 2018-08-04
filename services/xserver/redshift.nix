{ config, pkgs, ... }:
{
    services.redshift = {
        enable = true;
        latitude = "55.751244"; # TODO: extract constant
        longitude = "37.618423"; # TODO: extract constant
        temperature.day = 5500;
        temperature.night = 3700;
    };
}
