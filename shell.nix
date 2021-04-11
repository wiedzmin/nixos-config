{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
mkShell { buildInputs = [ cloc gitAndTools.git-crypt gitAndTools.pre-commit go-task nixUnstable nixfmt nixpkgs-fmt shfmt ]; }
