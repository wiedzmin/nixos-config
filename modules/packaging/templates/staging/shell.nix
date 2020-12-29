{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
in mkShell {
  buildInputs = [
  ];
  shellHook = ''
    echo
    echo -e "packages: "
    echo
  '';
}
