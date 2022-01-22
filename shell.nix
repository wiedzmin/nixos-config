{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
mkShell { buildInputs = [ cloc deadnix gitAndTools.git-crypt gitAndTools.pre-commit go-task just nixUnstable nixfmt nixpkgs-fmt shfmt ]; }
