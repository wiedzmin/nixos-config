{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        rustfmt
        rustracer
        rustup
    ];
}
