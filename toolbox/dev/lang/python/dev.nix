{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        (python3.withPackages (ps: with ps; [
           python-language-server
           python3Packages.pyls-black
           python3Packages.pyls-isort
           python3Packages.pyls-mypy
        ]))
        python3Packages.black
        python3Packages.pylama
        python3Packages.snakeviz
        python3Packages.virtualenvwrapper
    ];
}
