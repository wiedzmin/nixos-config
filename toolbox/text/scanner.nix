{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        deskew
        scantailor-advanced
        simple-scan
        xsane # temporarily kept for debug
    ];
}
