{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # TODO: unoconv
        aspell
        aspellDicts.en
        aspellDicts.ru
        ditaa
        djvu2pdf
        djvulibre
        ghostscript
        mupdf
        pandoc
        pdf2djvu
        pdf2djvu
        pdftk
        plantuml
        texlive.combined.scheme-full
        zathura
    ];
}
