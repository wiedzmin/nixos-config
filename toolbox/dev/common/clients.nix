{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        bash-language-server
        http-prompt
        httplab
        litecli # TODO: shell automation: fzf for selecting db file, you get the idea
        mycli
        nodePackages.elasticdump
        pgcenter
        pgcli
        redis-tui
        soapui
        wuzz
        zeal
    ];
}
