{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        clang
        cmake
        cquery
        llvm
    ];
}
