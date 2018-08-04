{ config, pkgs, ... }:

{
    imports = [
        ../contrib/custom-scripts.nix
    ];

    virtualisation.docker.enable = true;
    virtualisation.docker.storageDriver = "overlay2";

    virtualisation.libvirtd.enable = true;
    virtualisation.virtualbox.host.enable = true;
    nixpkgs.config.virtualbox.enableExtensionPack = true;

    environment.systemPackages = with pkgs; [
        ctop
        docker-machine
        docker_compose
        vagrant

        docker-machine-export
        docker-machine-import
    ];

    environment.shellAliases = {
        DI = "docker inspect";
        DP = "docker ps";
        DSH = "docker_shell(){ docker exec -it $1 /bin/bash }; docker_shell";
        DRM = "docker_stop_rm(){ docker stop $@ && docker rm $@ }; docker_stop_rm";
        DPA = "docker ps -a";
        DL = "docker logs";
        DPI = "docker_name_ip(){ docker ps --format '{{.Names}}' | grep $@ | xargs docker inspect -f '{{.Name}} {{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' | column -t -s' ' }; docker_name_ip";
        DIM = "docker inspect -f '{{ .Mounts }}'";
    };

    programs.zsh.shellAliases = {
        DI = "docker inspect";
        DP = "docker ps";
        DSH = "docker_shell(){ docker exec -it $1 /bin/bash }; docker_shell";
        DRM = "docker_stop_rm(){ docker stop $@ && docker rm $@ }; docker_stop_rm";
        DPA = "docker ps -a";
        DL = "docker logs";
        DPI = "docker_name_ip(){ docker ps --format '{{.Names}}' | grep $@ | xargs docker inspect -f '{{.Name}} {{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' | column -t -s' ' }; docker_name_ip";
        DIM = "docker inspect -f '{{ .Mounts }}'";
    };
}
