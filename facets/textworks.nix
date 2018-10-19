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
        aspell
        aspellDicts.en
        aspellDicts.ru
        ditaa
        djview
        djvu2pdf
        djvulibre
        ghostscript
        mupdf
        noweb
        pandoc
        pdf2djvu
        pdf2djvu
        pdftk
        plantuml
        texlive.combined.scheme-full
        zathura
        ansifilter
    ] ++ [ # scanner
        xsane # temporarily kept for debug
        simple-scan
        scantailor-advanced
    ];
}
