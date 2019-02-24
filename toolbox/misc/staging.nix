{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # broot
        # go-check
        # gscan2pdf
        # nix-gitignore
        # pyflame
        bbe
        clerk
        gitAndTools.git-imerge
        gitAndTools.gita # TODO: review https://github.com/nosarthur/gita + some respective rofi scripts
        gitAndTools.tig
        k6
        nix-prefetch
    ];
}
