{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        rls
        rustfmt
        rustracer
        rustup
    ];
}
