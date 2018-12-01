{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        rustracer
        rustup
    ];
}
