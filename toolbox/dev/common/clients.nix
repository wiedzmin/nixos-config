{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        corgi # TODO: make custom script to open fuzzy seaching of snippets in new tmux pane
        http-prompt
        httplab
        litecli # TODO: shell automation: fzf for selecting db file, you get the idea
        mycli
        nodePackages.elasticdump
        pgcenter
        pgcli
        wuzz
        zeal
    ];
}
