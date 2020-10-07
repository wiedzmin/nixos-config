{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
mkShell { buildInputs = [ gitAndTools.pre-commit gnumake nixUnstable nixfmt shfmt ]; }
