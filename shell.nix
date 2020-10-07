{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
mkShell { buildInputs = [ gitAndTools.pre-commit make nixUnstable nixfmt shfmt ]; }
