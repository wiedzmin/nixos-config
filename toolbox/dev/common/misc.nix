{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        ansible
        ansible-lint
        cloc
        crex
        dotnet-sdk
        doxygen
        global
        highlight
        hyperfine
        sloccount
        vim
        weighttp
        pciutils
    ];
}
