{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        fpart
        gtm
        jdupes
        todoman
    ];
}
