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
        just
        k6
        mkcert
        sloccount
        tcpreplay
        vim
        vscode
        weighttp
        pciutils
    ];
}
