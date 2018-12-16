{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        flow
        nodePackages.javascript-typescript-langserver
        nodePackages.node2nix
    ];
}
