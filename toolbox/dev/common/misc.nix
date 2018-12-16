{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        ansible
        ansible-lint
        buck
        cloc
        doxygen
        global
        highlight
        hyperfine
        sloccount
        vim
        weighttp
    ];
}
