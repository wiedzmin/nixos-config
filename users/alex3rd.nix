{pkgs, ...}:
{
  imports = [
  ];
  users.extraUsers = {
      alex3rd = {
          isNormalUser = true;
          uid = 1000;
          description = "Alex Ermolov";
          shell = pkgs.zsh;
          extraGroups = [ "audio" "docker" "lp" "networkmanager" "scanner" "vboxusers" "video" "wheel" ];
      };
  };
  programs.zsh = {
      enable = true;
      ohMyZsh = {
          enable = true;
          plugins = [
              "colored-man-pages"
              "dirpersist"
          ];
          theme = "simple";
      };
      autosuggestions.enable = true;
      enableCompletion = true;
      interactiveShellInit = ''
          PATH=$PATH:${pkgs.autojump}/bin
          . ${pkgs.autojump}/share/autojump/autojump.zsh
      '';
  };
  programs.zsh.zsh-autoenv = {
      enable = true;
      package = pkgs.zsh-autoenv;
  };
  programs.bash.enableCompletion = true;
}
