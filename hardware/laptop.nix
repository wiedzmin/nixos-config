{ lib, pkgs, ... }:

{
    powerManagement = {
        enable = true;
    };
    services.tlp.enable = true;
}
