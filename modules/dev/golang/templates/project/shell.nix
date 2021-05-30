{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
  base = [ codesearch docker_compose gitAndTools.pre-commit just watchman ];
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
in
mkShell {
  buildInputs = base ++ stats ++ git ++ [
    go-bindata
    goconvey
    golangci-lint
    gomodifytags
    gopkgs
    gotools
    nurpkgs.wiedzmin.go-mod-outdated
    nurpkgs.wiedzmin.gohack
  ];
  shellHook = ''
    [ -f "./.aux" ] && source ./.aux
  '';
}
