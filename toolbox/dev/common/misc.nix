{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        ansible
        ansible-lint
        buck
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
