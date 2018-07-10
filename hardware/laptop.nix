{ lib, pkgs, ... }:

{
    powerManagement = {
        enable = true;
    };
    services.tlp.enable = true;
    services.acpid.enable = true;
}
