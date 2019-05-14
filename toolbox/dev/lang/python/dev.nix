{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        ms-pyls
        python3Packages.snakeviz
        python3Packages.virtualenv
        python3Packages.virtualenvwrapper
        yapf
    ];
}
