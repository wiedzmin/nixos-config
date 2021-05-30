{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
  base = [ gitAndTools.git just ]
  media = [ youtube-dl ffmpeg ]
  misc = [ gospider python3Packages.pytube ]
in
mkShell {
  buildInputs = base ++ media ++ misc ++ [ ];
  shellHook = ''
    [ -f "./.aux" ] && source ./.aux
  '';
}
