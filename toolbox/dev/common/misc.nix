{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        ansible
        ansible-lint
        cloc
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
