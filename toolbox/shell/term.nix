{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        alacritty
        eternal-terminal
        tmux
    ];
}
