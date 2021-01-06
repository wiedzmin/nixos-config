{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ide.emacs.navigation;
  user = config.attributes.mainUser.name;
in {
  options = {
    ide.emacs.navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs navigation extensions.";
      };
      ivy.candidatesCount = mkOption {
        type = types.int;
        default = 10;
        description = "Candidates count to display for Ivy completion engine.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "emacs: core configuration must be enabled.";
      }];

      home-manager.users.${user} = { home.packages = with pkgs; [ ripgrep ]; };
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.ace-link
        epkgs.ace-window
        epkgs.amx
        epkgs.avy
        epkgs.avy-flycheck
        epkgs.avy-zap
        epkgs.beginend
        epkgs.block-nav
        epkgs.counsel
        epkgs.counsel-jq
        epkgs.counsel-projectile
        epkgs.counsel-tramp
        epkgs.dired-filetype-face
        epkgs.dired-git-info
        epkgs.dired-hide-dotfiles
        epkgs.dired-launch
        epkgs.dired-narrow
        epkgs.dired-quick-sort
        epkgs.flycheck-projectile
        epkgs.goggles
        epkgs.imenu-anywhere
        epkgs.ivy
        epkgs.ivy-avy
        epkgs.ivy-historian
        epkgs.ivy-rich
        epkgs.ivy-xref
        epkgs.ivy-yasnippet
        epkgs.link-hint
        epkgs.mwim
        epkgs.phi-search
        epkgs.phi-search-mc
        epkgs.polymode
        epkgs.projectile
        epkgs.rainbow-delimiters
        epkgs.recursive-narrow
        epkgs.rg
        epkgs.swiper
        epkgs.treemacs
        epkgs.treemacs-projectile
      ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./navigation.el;
    })
  ];
}
