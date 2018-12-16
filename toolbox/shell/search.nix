{ config, pkgs, ... }:

{
    services.locate.enable = true;

    environment.systemPackages = with pkgs; [
        exa
        fd
        findutils
        rdfind
        ripgrep
    ];
}
