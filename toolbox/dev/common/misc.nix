{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        cloc
        dotnet-sdk              # for building some binary releases
        hyperfine
        just
        k6
        mkcert
        pciutils
        sloccount
        tcpreplay
        vim
        vscode
        weighttp
    ];
}
