{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ide.emacs.edit;
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    ide.emacs.edit = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable editing extensions";
      };
      autorevert.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable autorevert package";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "emacs/edit: core configuration must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.aggressive-indent
        epkgs.drag-stuff
        epkgs.edit-indirect
        epkgs.evil-nerd-commenter
        epkgs.focus
        epkgs.highlight-numbers
        epkgs.multiple-cursors
        epkgs.region-bindings-mode
        epkgs.shift-number
        epkgs.smartparens
        epkgs.string-inflection
        epkgs.symbol-overlay
        epkgs.symbol-overlay-mc
        epkgs.undo-tree
        epkgs.wgrep
        epkgs.whole-line-or-region
        epkgs.ws-butler
      ] ++ optionals (!config.ide.emacs.core.treesitter.enable && !config.pim.core.emacs.automation.enable) [
        epkgs.expand-region
      ] ++ optionals (config.ide.emacs.core.treesitter.enable && !config.pim.core.emacs.automation.enable) [
        epkgs.expreg
      ] ++ optionals (config.ide.emacs.core.treesitter.enable) [
        epkgs.treesit-fold
      ];
      ide.emacs.core.config =
        readSubstituted config inputs pkgs [ ./subst/edit.nix ] [ ./elisp/edit.el ] +
        optionalString (!config.ide.emacs.core.treesitter.enable) (builtins.readFile ./elisp/non-ts.el) +
        optionalString (config.ide.emacs.core.treesitter.enable) (builtins.readFile ./elisp/ts.el) +
        optionalString (!config.ide.emacs.core.treesitter.enable && !config.pim.core.emacs.automation.enable) ''
          (use-package expand-region
            :bind
            ("C-," . er/expand-region)
            ("C-." . er/contract-region)
            :config
            (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
            (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode))
        '' +
        optionalString (config.ide.emacs.core.treesitter.enable && !config.pim.core.emacs.automation.enable) ''
          (use-package expreg
            :bind
            ("C-," . expreg-expand)
            ("C-." . expreg-contract))
        '';
      ide.emacs.core.customKeymaps = {
        "misc-editing-map" = "<f5>";
        "token-editing-map" = "C-z";
      };
      ide.emacs.core.customPackages = {
        "edit-misc" = { text = builtins.readFile ./elisp/custom/edit-misc.el; };
        "minibuffer-edit" = { text = builtins.readFile ./elisp/custom/minibuffer-edit.el; };
      };
      ide.emacs.core.treesitter.grammars = {
        toml = "https://github.com/tree-sitter/tree-sitter-toml";
        yaml = "https://github.com/ikatyang/tree-sitter-yaml";
      };
      ide.emacs.core.treesitter.modeRemappings = {
        conf-toml-mode = "toml-ts-mode";
        yaml-mode = "yaml-ts-mode";
      };
    })
    (mkIf (cfg.enable && cfg.autorevert.enable) {
      ide.emacs.core.config = builtins.readFile ./elisp/autorevert.el;
    })
  ];
}
