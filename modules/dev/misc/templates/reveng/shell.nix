{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
  base = [ gitAndTools.pre-commit gnumake ];
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
  reveng = [
    angr
    binwalk
    hachoir
    radare2
    elf-dissector
  ];
in mkShell {
  buildInputs = base ++ git ++ reveng ++ [ ];
}
