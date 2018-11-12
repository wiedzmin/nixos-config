{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        nix-index
        nix-prefetch-scripts
        nix-prefetch-github
        nix-serve
        nixops
        nix-du
    ];
}
