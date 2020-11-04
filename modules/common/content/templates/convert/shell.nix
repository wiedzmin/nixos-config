{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos.wiedzmin;
in
mkShell {
  buildInputs = [
    gitAndTools.git

    youtube-dl
    ffmpeg
  ];
  shellHook = ''
    echo
    echo -e "yt/download - download from Youtube"
    echo -e "filelist - prepare list of media files for ffmpeg"
    echo
  '';
}
