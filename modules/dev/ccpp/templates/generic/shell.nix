{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
  base = [ autoconf automake bear bison codesearch flex libtool ninja pkg-config ];
  stats = [ cloc gource logtop sloccount tokei ];
  env = [ gitAndTools.pre-commit gnumake ];
  git = [
    git-quick-stats
    git-sizer
    gitAndTools.git-chglog
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
  buildInputs = env ++ base ++ stats ++ git ++ [ uncrustify debugedit-unstable ];
  shellHook = ''
    [ -f "./.aux" ] && source ./.aux
  '';
}
