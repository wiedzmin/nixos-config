{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        clang
        cquery
        llvm
        ninja
        rtags
    ];
}
