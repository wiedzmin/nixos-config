{ config, pkgs, ... }:
{
  imports = [ ./emacs ./gui.nix ./shell.nix ./xmonad ];

  home-manager.users."${config.attributes.mainUser.name}" = {
    programs.ssh = {
      enable = true;
      forwardAgent = true;
      userKnownHostsFile = "~/.ssh/known_hosts";
      controlMaster = "auto";
      controlPath = "~/.ssh/sockets/%r@%h:%p";
      controlPersist = "4h";
      serverAliveInterval = 30;
    };
  };
}
