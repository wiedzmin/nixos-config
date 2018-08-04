{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        python3Full
        python2Full
        pypi2nix

        # python3Packages.rst2pdf # TODO: add derivation
        # python3Packages.traad # TODO: add derivation
        python3Packages.GitPython
        python3Packages.autopep8
        python3Packages.flake8
        python3Packages.importmagic
        python3Packages.isort
        python3Packages.jedi
        python3Packages.jmespath
        python3Packages.notebook
        python3Packages.olefile
        python3Packages.pep8
        python3Packages.pylint
        python3Packages.python-language-server
        python3Packages.snakeviz
        python3Packages.virtualenv
        python3Packages.virtualenvwrapper
        python3Packages.yapf

        pkgs.idea.pycharm-community
    ];
}
