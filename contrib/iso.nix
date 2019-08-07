{config, pkgs, ...}:
let
    custom = import ../../../../pkgs/custom pkgs config;
in
{
    imports = [
        <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
        <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
    ];

    config = {
        environment.systemPackages = with pkgs; [
            custom.bootstrap_custom_config
        ];
    };
}
