{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        gnutar
        p7zip
        unrar
        unzip
        zip
    ];

    environment.shellAliases = {
        untar = "tar xvvf";
    };

    programs.zsh.shellAliases = {
        untar = "tar xvvf";
    };
}
