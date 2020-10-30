{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos.wiedzmin;
in
mkShell {
  buildInputs = [
    docker_compose
    gitAndTools.pre-commit
    gnumake
    watchman

    errcheck
    go-bindata
    goconvey
    golangci-lint
    gomodifytags
    gopkgs
    gotools
    nurpkgs.gohack
  ];
  shellHook = ''
    echo
    echo -e "build - build project main module"
    echo -e "build/docker - build Docker image"
    echo -e "generate - generate boilerplate"
    echo -e "lint - lint sources (golangci-lint)"
    echo -e "lint/fix - lint+fix sources (golangci-lint)"
    echo -e "deps/download - download dependencies"
    echo -e "deps/update - updated dependencies to recent versions"
    echo -e "deps/gc - remove unused dependencies"
    echo -e "publish - push current results"
    echo -e "publish/force - push current results (is needed occasionally)"
    echo
  '';
}
