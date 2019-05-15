{ config, pkgs, ... }:

{
    services.locate.enable = true;

    environment.systemPackages = with pkgs; [
        rdfind
    ];
}
