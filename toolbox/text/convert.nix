{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # TODO: unoconv
        ansifilter
        ditaa
        djvu2pdf
        gron
        help2man
        noweb
        ntangle
        pandoc
        par
        pdf2djvu
        pdftk
        plantuml
        scdoc
    ];
}
