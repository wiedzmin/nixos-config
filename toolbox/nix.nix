{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        nix-du
        nix-index
        nix-prefetch-github
        nix-prefetch-scripts
        nix-serve
        nixops

        # shell completions
        nix-bash-completions
        nix-zsh-completions
    ];
}
