{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ide.emacs.navigation;
  user = config.attributes.mainUser.name;
in
{
  options = {
    ide.emacs.navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs navigation extensions.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "emacs/navigation: core configuration must be enabled.";
      }];

      home-manager.users.${user} = { home.packages = with pkgs; [ ripgrep ]; };
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.avy
        epkgs.beginend
        epkgs.block-nav
        epkgs.consult
        epkgs.consult-dir
        epkgs.consult-flycheck
        epkgs.dired-filetype-face
        epkgs.dired-narrow
        epkgs.embark
        epkgs.embark-consult
        epkgs.flycheck-projectile
        epkgs.goggles
        epkgs.imenu-anywhere
        epkgs.link-hint
        epkgs.magit-todos
        epkgs.manage-minor-mode-table
        epkgs.marginalia
        epkgs.mwim
        epkgs.orderless
        epkgs.phi-search
        epkgs.phi-search-mc
        epkgs.polymode
        epkgs.prescient
        epkgs.projectile
        epkgs.rainbow-delimiters
        epkgs.recursive-narrow
        epkgs.rg
        epkgs.consult-projectile
        epkgs.selectrum
        epkgs.selectrum-prescient
        epkgs.treemacs
        epkgs.treemacs-projectile
        epkgs.winum
      ];
      ide.emacs.core.customPackages = {
        "minibuffer-edit" = builtins.readFile ./custom/minibuffer-edit.el;
        "orderless-dispatchers" = builtins.readFile ./custom/orderless-dispatchers.el;
      };
      ide.emacs.core.config = readSubstituted [ ./subst.nix ] [ ./navigation.el ];
      ide.emacs.core.customKeymaps = {
        "custom-help-map" = "<f1>";
        "custom-narrowing-map" = "C-c n";
        "custom-nav-map" = "C-q";
        "custom-projectile-map" = "<f8>";
        "custom-treemacs-map" = "C-x t";
        "custom-ws-map" = "C-c x";
        "custom-goto-map" = "M-s";
        # TODO: remap original goto-map back to global keys space, because of current accessibility reduce for:
        # simple:
        # ("n" . next-error)
        # ("p" . previous-error)
        # ("M-n" . next-error)
        # ("M-p" . previous-error)
        # emacs:
        # ("TAB" . move-to-column)
        # ("c" . goto-char)
      };
      # TODO: consider enhance custom keymaps nix machinery in a way of automatically unbinding
      # either global or some submapped confilcting keybindings
    })
  ];
}
