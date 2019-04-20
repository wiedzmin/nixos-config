{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        git
        git-quick-stats
        git-sizer
        git-town
        gitAndTools.ghq
        gitAndTools.git-absorb
        gitAndTools.git-extras
        gitAndTools.git-ignore
        gitAndTools.git-imerge
        gitAndTools.git-open
        gitAndTools.git-secret
        gitAndTools.git-secrets
        gitAndTools.pass-git-helper
        icdiff
        stgit
    ];
}

# TODO: https://github.com/awslabs/git-secrets add to hooks
