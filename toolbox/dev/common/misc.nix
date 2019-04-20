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
        jsonnet
        k6
        sloccount
        tcpreplay
        vim
        vscode
        weighttp
        pciutils
    ];
}
