{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        bc
        dateutils
        dex
        doitlive
        gcalcli
        loop
        mc
        plan9port
        renameutils
        replace
        shellcheck
        tree
        unicode-paracode
        wtf
    ];
}
