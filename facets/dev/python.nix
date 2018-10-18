{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # infra
        pypi2nix
        python2Full
        python3Full

        # dev
        idea.pycharm-community
        pyre
        python3Packages.GitPython
        python3Packages.autopep8
        python3Packages.black
        python3Packages.flake8
        python3Packages.importmagic
        python3Packages.isort
        python3Packages.jedi
        python3Packages.jmespath
        python3Packages.mccabe
        python3Packages.notebook
        python3Packages.olefile
        python3Packages.pep8
        python3Packages.pycodestyle
        python3Packages.pydocstyle
        python3Packages.pyflakes
        python3Packages.pylint
        python3Packages.pyls-black
        python3Packages.pyls-isort
        python3Packages.pyls-mypy
        python3Packages.python-language-server
        python3Packages.rope
        python3Packages.snakeviz
        python3Packages.virtualenv
        python3Packages.virtualenvwrapper
        python3Packages.yapf
    ];
}
