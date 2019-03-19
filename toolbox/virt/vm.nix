{ config, pkgs, ... }:

{
    virtualisation.libvirtd.enable = true;
    virtualisation.virtualbox.host.enable = true;
    virtualisation.virtualbox.host.enableExtensionPack = true;

    environment.systemPackages = with pkgs; [
        vagrant
    ];
}
