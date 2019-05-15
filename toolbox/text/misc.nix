{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        pdfgrep
        pdfsandwich
        ttyplot
        visidata # TODO: make overlay
        xurls
    ];
}
