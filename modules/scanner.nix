{ config, pkgs, ... }:

{
    nixpkgs.config = {
        sane.snapscanFirmware = "/etc/nixos/private/firmware/Esfw52.bin";
    };

    hardware.sane = {
        enable = true;
        # extraBackends = [ pkgs.epkowa ];
    };

    environment.systemPackages = with pkgs; [
        xsane # temporarily kept for debug
        simple-scan
        scantailor-advanced
    ];
}
