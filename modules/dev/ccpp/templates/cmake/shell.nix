{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  base = [ autoconf automake bear bison flex libtool pkg-config cmake ];
  env = [ gitAndTools.pre-commit gnumake ];
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
in mkShell { buildInputs = env ++ base ++ [ ]; }
