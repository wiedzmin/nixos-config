{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.git.navigation;
  user = config.attributes.mainUser.name;
in {
  options = {
    dev.git.navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git navigation setup.";
      };
      ghq.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable ghq tooling.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs navigation-related git setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.ghq.enable) {
      environment.systemPackages = with pkgs; [ gitAndTools.ghq ];
      home-manager.users.${user} = {
        programs.git.extraConfig = { "ghq" = { root = homePrefix config.navigation.bookmarks.workspaces.globalRoot; }; };
        programs.zsh.shellAliases = { gg = "${pkgs.gitAndTools.ghq}/bin/ghq get"; };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.dired-git-info
        epkgs.git-timemachine
        epkgs.git-walktree
        epkgs.magit-todos
        epkgs.treemacs-magit
      ];
      ide.emacs.core.config = readSubstituted ../../../subst.nix ./emacs/navigation.el;
    })
    (mkIf (cfg.enable && cfg.ghq.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.helm-ghq ];
      ide.emacs.core.config = readSubstituted ../../../subst.nix ./emacs/ghq.el;
    })
  ];
}
