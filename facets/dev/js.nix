{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        flow
    ];
}
