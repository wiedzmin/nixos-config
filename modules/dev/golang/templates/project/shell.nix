{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
  base = [ docker_compose gitAndTools.pre-commit gnumake watchman ];
  stats = [ cloc gource logtop sloccount tokei ];
  git = [
    git-quick-stats
    git-sizer
    gitAndTools.git-filter-repo
    gitAndTools.git-machete
    gitAndTools.git-reparent
    gitAndTools.git-subset
    gitAndTools.git-trim
    gitstats
    gomp
  ];
in mkShell {
  buildInputs = base ++ stats ++ git ++ [
    go-bindata
    goconvey
    golangci-lint
    gomodifytags
    gopkgs
    gotools
    nurpkgs.wiedzmin.gohack
  ];
  shellHook = ''
    [ -f "./.aux" ] && source ./.aux
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
