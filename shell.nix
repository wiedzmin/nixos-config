{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
mkShell { buildInputs = [ cloc gitAndTools.git-crypt gitAndTools.pre-commit gnumake nixUnstable nixfmt shfmt ]; }
