{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        buku
        fpart
        jdupes
        rmlint
    ];
}
