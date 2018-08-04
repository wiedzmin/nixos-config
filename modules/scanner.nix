{ config, pkgs, ... }:

{
    nixpkgs.config = {
        packageOverrides = pkgs: {
           xsaneGimp = pkgs.xsane.override { gimpSupport = true; };
        };
        sane.snapscanFirmware = "/etc/nixos/private/firmware/Esfw52.bin";
    };

    hardware.sane = {
        enable = true;
        # extraBackends = [ pkgs.epkowa ];
    };

    environment.systemPackages = with pkgs; [
        xsane
    ];
}
