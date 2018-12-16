{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        pypi2nix
        python2Full
        python3Full
    ];
}
