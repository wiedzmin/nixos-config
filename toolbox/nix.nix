{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        gen-nix-du
        nix-index # TODO: maybe make easier shell alias
        nix-prefetch
        nix-prefetch-github
        nix-prefetch-scripts

        nix-zsh-completions
    ];
}
