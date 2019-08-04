{ config, pkgs, lib, ... }:
with import ../../const.nix {inherit config pkgs;};
{
    virtualisation.docker = {
        enable = true;
        storageDriver = "overlay2";
    };

    virtualisation.libvirtd = {
        enable = true;
    };

    users.extraUsers."${userName}".extraGroups = [ "docker" "libvirtd" "vboxusers" ];
}
