{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        go
        dep2nix
        go2nix
    ];
}
