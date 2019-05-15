{ config, pkgs, ... }:

{
    virtualisation.docker = {
        enable = true;
        storageDriver = "overlay2";
    };

    environment.systemPackages = with pkgs; [
        ctop
        dive
        docker-machine
        docker_compose
        libcgroup
        skopeo
    ];
}
