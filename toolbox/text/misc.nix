{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        aspell
        aspellDicts.en
        aspellDicts.ru
        tabula
        ttyplot
        visidata
        xurls
    ];
}
