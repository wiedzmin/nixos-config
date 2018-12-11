{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        ansible
        ansible-lint
        atom
        binutils
        buck
        cloc
        corgi # TODO: make custom script to open fuzzy seaching of snippets in new tmux pane
        doxygen
        elfinfo
        flamegraph
        gdb
        gdbgui
        gist
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
        global
        gtm
        highlight
        httplab
        idutils
        insomnia
        jid
        jq
        lazygit
        ltrace
        mercurial
        mycli
        ntfy
        patchelf
        patchutils
        pgcli
        reflex
        sloccount
        stgit
        valgrind
        vim
        vscode
        watchexec
        weighttp
        wuzz
        zeal

        # reveng
        radare2
        radare2-cutter
        retdec
        rr
    ];
}
