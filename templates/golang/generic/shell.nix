{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
mkShell {
  buildInputs = [ gnumake watchman ];
  shellHook = ''
    echo
    echo -e ">>> placeholder <<<"
    echo
  '';
}
