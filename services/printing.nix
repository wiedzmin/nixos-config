{ config, pkgs, ... }:

{
    services.printing = {
        enable = true;
        drivers = [ pkgs.hplip ];
    };
}
