{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        xsv
    ];
}
