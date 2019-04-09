{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # broot
        # cdimgtools
        # go-check
        # gscan2pdf
        # nix-gitignore
        # pyflame
        anbox
        bbe
        clerk
        findnewest
        git-town
        gitAndTools.git-ignore
        gitAndTools.git-imerge
        jsonnet
        k6
        libwhich
        nix-prefetch
        pdfsandwich
        semver-tool
        sysdig
        vmtouch
        websocat
        xtruss
        zzuf

        nodePackages.indium
    ];
}
