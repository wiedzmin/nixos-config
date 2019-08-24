{ bash, config, coreutils, dunst, git, pkgs, ... }:
with import ../const.nix { inherit config pkgs; };
''
  #!${bash}/bin/bash

  current_system_commit_hash=`${coreutils}/bin/readlink -f /run/current-system | ${coreutils}/bin/cut -f4 -d.`
  cd ${nixpkgsFullPath}
  nixpkgs_current_branch=$(${git}/bin/git symbolic-ref --short HEAD)
  cd ${homeManagerFullPath}
  hm_current_branch=$(${git}/bin/git symbolic-ref --short HEAD)
  hm_current_hash=$(${git}/bin/git rev-parse --short HEAD)
  ${dunst}/bin/dunstify -t 15000 "nixpkgs: $current_system_commit_hash/$nixpkgs_current_branch
  HM: $hm_current_hash/$hm_current_branch"
''
