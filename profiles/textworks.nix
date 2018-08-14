{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        aspell
        aspellDicts.en
        aspellDicts.ru
        ditaa
        pandoc
        pdf2djvu
        pdftk
        plantuml
        texlive.combined.scheme-full
        zathura
    ];
}
