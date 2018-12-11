{ config, pkgs, ... }:

{
    imports = [
        ../scripts/virt.nix
    ];

    virtualisation.docker = {
        enable = true;
        storageDriver = "overlay2";
    };

    virtualisation.libvirtd.enable = true;
    virtualisation.virtualbox.host.enable = true;
    nixpkgs.config.virtualbox.enableExtensionPack = true;

    environment.systemPackages = with pkgs; [
        ctop
        docker-machine
        docker_compose
        skopeo
        vagrant

        docker-machine-export
        docker-machine-import
    ];
}
