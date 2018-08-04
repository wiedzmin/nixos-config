{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        nix-index
        nix-prefetch-scripts
        nix-repl
        nix-serve
        nixops
    ];
}
