{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        alacritty
        bc
        direnv
        exa
        fd # TODO: make `find` alias to this
        findutils
        mc
        multitail
        nix-bash-completions
        nix-zsh-completions
        psmisc
        pv
        renameutils
        replace
        ripgrep
        rxvt_unicode-with-plugins
        tmux
        tree
    ];

    environment.shellAliases = {
        li = "${pkgs.exa}/bin/exa -ial";
        lsd = "${pkgs.exa}/bin/exa -ld *(-/DN)";
        lsa = "${pkgs.exa}/bin/exa -ld .*";
    };

    programs.zsh.shellAliases = {
        li = "${pkgs.exa}/bin/exa -ial";
        lsd = "${pkgs.exa}/bin/exa -ld *(-/DN)";
        lsa = "${pkgs.exa}/bin/exa -ld .*";
    };
}
