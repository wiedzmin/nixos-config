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
        # terminals
        alacritty
        eternal-terminal
        kitty # TODO: review and compare with Alacritty
        rxvt_unicode-with-plugins
        tmux

        # nix-related
        nix-bash-completions
        nix-zsh-completions

        # view/search/transform
        bat
        bcat
        exa
        fd
        findutils
        fpp
        miller
        ripgrep
        tealdeer

        # misc
        bc
        dateutils
        dex
        doitlive
        gcalcli
        mc
        plan9port
        renameutils
        replace
        shellcheck
        tree
        unicode-paracode
        wtf
    ];
}
