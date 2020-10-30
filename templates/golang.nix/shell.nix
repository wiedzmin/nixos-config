{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
mkShell {
  buildInputs = [ git go go2nix ];
  shellHook = ''
    export GOPATH=$(pwd)

    echo
    echo -e ">>>>> instructions <<<<<"
    echo -e "go get <gitforge>/user/repo"
    echo -e "cd src/<gitforge>/user/repo"
    echo -e "go2nix save"
    echo
  '';
}
