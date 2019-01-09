{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        aspell
        aspellDicts.en
        aspellDicts.ru
        pdfgrep
        tabula
        ttyplot
        visidata
        xurls
    ];
}
