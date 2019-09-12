{ config, pkgs, lib, ... }:

{
  imports = [ ../../private/traits/common.nix ];

  home-manager.users.kotya = {
    programs.htop.enable = true;
    programs.command-not-found.enable = true;
    programs.lesspipe.enable = true;
    programs.man.enable = true;
    programs.info.enable = true;
    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };
  };
}
