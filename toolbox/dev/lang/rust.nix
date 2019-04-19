{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # rls                     # broken upstream
        rustfmt
        rustracer
        rustup
    ];
}
