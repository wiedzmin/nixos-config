{ config, pkgs, ... }:

{
    services.printing = {
        enable = true;
        drivers = [ pkgs.hplip ];
    };

    # scanner
    nixpkgs.config = {
        sane.snapscanFirmware = "/etc/nixos/private/firmware/Esfw52.bin";
    };

    hardware.sane = {
        enable = true;
        # extraBackends = [ pkgs.epkowa ];
    };

    environment.systemPackages = with pkgs; [
        # TODO: unoconv
        ansifilter
        aspell
        aspellDicts.en
        aspellDicts.ru
        ditaa
        djview
        djvu2pdf
        djvulibre
        fbreader
        ghostscript
        mupdf
        noweb
        ntangle
        pandoc
        par
        pdf2djvu
        pdftk
        plantuml
        texlive.combined.scheme-full
        zathura
    ] ++ [ # scanner
        xsane # temporarily kept for debug
        simple-scan
        scantailor-advanced
    ];
}
