{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        dep
        dep2nix
        glide
        go
    ];
}
