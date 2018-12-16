{ config, pkgs, ... }:

{
    virtualisation.docker = {
        enable = true;
        storageDriver = "overlay2";
    };

    environment.systemPackages = with pkgs; [
        ctop
        docker-machine
        docker_compose
        kind
        skopeo
    ];
}
