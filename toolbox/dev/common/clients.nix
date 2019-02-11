{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        corgi # TODO: make custom script to open fuzzy seaching of snippets in new tmux pane
        http-prompt
        httplab
        insomnia
        mycli
        nodePackages.elasticdump
        pgcli
        wuzz
        zeal
    ];
}
