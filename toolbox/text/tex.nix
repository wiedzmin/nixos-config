{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        git-latexdiff
        rubber
        texlive.combined.scheme-full
        texmaker
        texstudio
    ];
}
