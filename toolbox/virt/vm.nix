{ config, pkgs, ... }:

{
    virtualisation.libvirtd.enable = true;
    virtualisation.virtualbox.host.enable = true;
    nixpkgs.config.virtualbox.enableExtensionPack = true;

    environment.systemPackages = with pkgs; [
        vagrant
    ];
}
