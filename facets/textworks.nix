{ config, pkgs, ... }:

{
    services.printing = {
        enable = true;
        drivers = [ pkgs.hplip ];
    };

    # scanner
    nixpkgs.config = {
        sane.snapscanFirmware = "/etc/nixos/contrib/blobs/Esfw52.bin";
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
        calibre
        ditaa
        djview
        djvu2pdf
        djvulibre
        ghostscript
        gron
        noweb
        ntangle
        pandoc
        par
        pdf2djvu
        pdftk
        plantuml
        polar-bookshelf
        rubber
        tabula
        texlive.combined.scheme-full
        texmaker
        ttyplot
        visidata
        zathura
    ] ++ [ # scanner
        xsane # temporarily kept for debug
        simple-scan
        scantailor-advanced
    ];
}
