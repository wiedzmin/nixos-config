{ config, pkgs, ... }:

{
    imports = [
        ../../packages/git-quick-stats.nix
    ];

    environment.systemPackages = with pkgs; [
        ansible
        atom
        doxygen
        gdb
        gdbgui
        git
        git-lfs
        git-quick-stats
        gitAndTools.git-extras
        gitAndTools.git-imerge
        gitAndTools.git-secret
        gitAndTools.git-secrets
        gitAndTools.gitflow
        gitstats
        global
        highlight
        httplab
        idutils
        jq
        mercurial
        patchutils
        reflex
        skopeo
        sloccount
        vim
        vscode
        wuzz
        zeal

        # reveng
        radare2
        radare2-cutter
    ];

    environment.shellAliases = {
        ptch = "${pkgs.patch}/bin/patch -Ntbp0 < ";
        uptch = "${pkgs.patch}/bin/patch -NRtbp0 < ";
        clptch = "${pkgs.findutils}/bin/find . -name \*.orig -o -name \*.rej | xargs rm";
    };

    programs.zsh.shellAliases = {
        ptch = "${pkgs.patch}/bin/patch -Ntbp0 < ";
        uptch = "${pkgs.patch}/bin/patch -NRtbp0 < ";
        clptch = "${pkgs.findutils}/bin/find . -name \*.orig -o -name \*.rej | xargs rm";
    };
}
