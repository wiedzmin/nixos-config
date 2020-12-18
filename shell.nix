{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
mkShell { buildInputs = [ gitAndTools.git-crypt gitAndTools.pre-commit gnumake nixUnstable nixfmt shfmt ]; }
