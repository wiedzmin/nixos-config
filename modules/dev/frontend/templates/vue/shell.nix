{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
  base = [ docker_compose gitAndTools.pre-commit gnumake ];
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
    nodejs-14_x
    yarn
  ];
  # TODO: add automation for updating shellHook after merging Makefiles
  shellHook = ''
    [ -f "./identity" ] && source ./identity
    chmod +x ./merge-makefiles.sh
    echo
    echo -e "build/docker - build Docker image"
    echo -e "publish - push current results"
    echo -e "publish/force - push current results (is needed occasionally)"
    echo
  '';
}
