{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        bear
        clang
        cmake
        cquery
        llvm
        ninja
        rtags
    ];
}
