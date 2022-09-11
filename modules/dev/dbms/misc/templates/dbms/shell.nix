{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
in
mkShell {
  buildInputs = [
    litecli
    sqlitebrowser
    nodePackages.elasticdump
  ];
}
