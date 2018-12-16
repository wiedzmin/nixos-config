{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        corgi # TODO: make custom script to open fuzzy seaching of snippets in new tmux pane
        gist
        http-prompt
        httpie
        httplab
        insomnia
        mycli
        pgcli
        wuzz
        zeal
    ];
}
