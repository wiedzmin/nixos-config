{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        pandoc
        texlive.combined.scheme-full
        aspell
        aspellDicts.en
        aspellDicts.ru
        ditaa
        pdf2djvu
        pdftk
        plantuml
        zathura
    ];
}
