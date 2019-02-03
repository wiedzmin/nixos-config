{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        idea.pycharm-community
        pyflame
        pyre
        python3Packages.python-language-server
        python3Packages.black
        python3Packages.pylama
        python3Packages.snakeviz
        python3Packages.virtualenvwrapper
    ];
}
