{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        miller
        xsv
    ];
}
