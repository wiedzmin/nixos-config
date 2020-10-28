{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  base = [ autoconf automake bear bison flex libtool pkg-config ];
  env = [ gitAndTools.pre-commit gnumake ];
in
mkShell {
  buildInputs = env ++ base ++ [ ];
}
