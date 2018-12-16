{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        git
        git-lfs
        git-quick-stats
        git-sizer
        gitAndTools.ghq
        gitAndTools.git-absorb
        gitAndTools.git-extras
        gitAndTools.git-imerge
        gitAndTools.git-open
        gitAndTools.git-recent
        gitAndTools.git-secret
        gitAndTools.git-secrets
        gitAndTools.gitflow
        gitAndTools.pass-git-helper
        gitinspector
        gitstats
        mercurial
        stgit
    ];
}
