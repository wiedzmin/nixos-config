{ pkgs ? import <nixpkgs> { }, ... }:

# TODO: nurpkgs.dephell
# TODO: python3Packages.poetry

# TODO: review if we need them:
# python3Packages.autopep8
# python3Packages.importmagic
# python3Packages.virtualenv
# python3Packages.virtualenvwrapper
# python3Packages.yapf
# prospector # TODO: review configuration https://github.com/PyCQA/prospector

with pkgs;
mkShell {
  buildInputs = [ docker_compose gitAndTools.pre-commit gnumake golangci-lint gotools watchman ];
  shellHook = ''
    echo
    echo -e "publish - push current results"
    echo -e "publish/force - push current results (is needed occasionally)"
    echo
  '';
}
