{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        clang
        llvm
        ninja
        rtags
    ];
}
