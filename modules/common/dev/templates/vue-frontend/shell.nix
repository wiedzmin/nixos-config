{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
in
mkShell {
  buildInputs = [
    docker_compose
    gitAndTools.pre-commit
    gnumake

    nodejs-14_x
    yarn
  ];
  # TODO: add automation for updating shellHook after merging Makefiles
  shellHook = ''
    chmod +x ./merge-makefiles.sh
    echo
    echo -e "build/docker - build Docker image"
    echo -e "publish - push current results"
    echo -e "publish/force - push current results (is needed occasionally)"
    echo
  '';
}
