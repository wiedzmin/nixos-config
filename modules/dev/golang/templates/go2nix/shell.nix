{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
stdenv.mkDerivation rec {
  name = "generic-shell-go";
  env = buildEnv {
    inherit name;
    paths = buildInputs;
  };

  shellHook = ''
    echo
    echo -e ">>>>> instructions <<<<<"
    {{ if not .golangEnableModules }}
    export GOPATH="$(pwd):$GOPATH"
    export GO111MODULE=off
    echo -e "go get <gitforge>/user/repo"
    echo -e "cd src/<gitforge>/user/repo"
    echo -e "go2nix save"
    {{ else }}
    echo -e "vgo2nix"
    {{ end }}
    echo
  '';

  buildInputs = [ git go go2nix vgo2nix ];
}
