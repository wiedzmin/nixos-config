{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        nix-index
        nix-prefetch-scripts
        nix-serve
        nixops
    ];
}
