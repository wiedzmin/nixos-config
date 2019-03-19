{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        buku
        fpart
        gtm
        jdupes
        rmlint
    ];
}
