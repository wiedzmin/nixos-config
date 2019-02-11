{ config, pkgs, ... }:

{
    virtualisation.docker = {
        enable = true;
        storageDriver = "overlay2";
    };

    environment.systemPackages = with pkgs; [
        arion
        ctop
        dive
        docker-machine
        docker_compose
        img
        kind
        libcgroup
        oci-image-tool
        skopeo
    ];
}
