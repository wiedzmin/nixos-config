{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        git-quick-stats
        git-sizer
        gitAndTools.ghq
        gitAndTools.git-absorb # TODO: review abilities and maybe use in some automation
        gitAndTools.git-extras
        gitAndTools.git-imerge
        gitAndTools.pass-git-helper
        icdiff
    ];
}

# TODO: some shell implementation (maybe also in emacs/wm) of entrypoint of various tools above
