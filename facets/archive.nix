{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        gnutar
        unrar
        unzip
        zip
        p7zip
    ];

    environment.shellAliases = {
        untar = "tar xvvf";
    };

    programs.zsh.shellAliases = {
        untar = "tar xvvf";
    };
}
