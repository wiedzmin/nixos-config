{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        python2Packages.python-language-server
        python2Packages.pyls-isort
        python3Packages.python-language-server
        python3Packages.pyls-black
        python3Packages.pyls-isort
        python3Packages.pyls-mypy
        python3Packages.snakeviz
        python3Packages.virtualenvwrapper
        yapf
    ];
}
