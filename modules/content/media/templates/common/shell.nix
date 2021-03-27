{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
in mkShell {
  buildInputs = [
    gitAndTools.git

    youtube-dl
    ffmpeg
    gospider
  ];
  shellHook = ''
    echo
    echo -e "yt/download - download from Youtube"
    echo -e "filelist - prepare list of media files for ffmpeg"
    echo
  '';
}
