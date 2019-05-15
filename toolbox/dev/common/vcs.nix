{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        git-quick-stats
        git-sizer
        gitAndTools.ghq
        gitAndTools.git-absorb
        gitAndTools.git-extras
        gitAndTools.git-imerge
        gitAndTools.pass-git-helper
        icdiff
    ];
}

