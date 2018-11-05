{ config, pkgs, ... }:

{
    services.kmscon = {
        enable = true;
        hwRender = true;
        extraConfig = ''
            font-name=Iosevka Bold
            font-size=14
        '';
        extraOptions = "--term xterm-256color";
        autologinUser = "alex3rd";
    };

    services.locate.enable = true;
    services.urxvtd.enable = true;

    environment.systemPackages = with pkgs; [
        alacritty
        bat
        bc
        exa
        fd # TODO: make `find` alias to this
        findutils
        fpp
        htop
        iotop
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
        shellcheck
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
