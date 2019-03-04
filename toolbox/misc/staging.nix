{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # broot
        # cdimgtools
        # go-check
        # gscan2pdf
        # nix-gitignore
        # pyflame
        bbe
        clerk
        gitAndTools.git-ignore
        gitAndTools.git-imerge
        jsonnet
        k6
        nix-prefetch
        sysdig
    ];
}
