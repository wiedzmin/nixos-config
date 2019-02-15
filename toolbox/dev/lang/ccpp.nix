{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        clang
        cmake
        ccls
        llvm
    ];
}
