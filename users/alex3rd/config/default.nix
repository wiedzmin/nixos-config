{ config, pkgs, ... }:
with import ../const.nix { inherit config pkgs; }; {
  imports = [ ./dev ./emacs ./email.nix ./gui.nix ./shell.nix ./xmonad ];

  home-manager.users."${userName}" = {
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
