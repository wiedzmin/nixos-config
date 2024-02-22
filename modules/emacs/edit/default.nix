{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ide.emacs.edit;
  user = config.attributes.mainUser.name;
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
        epkgs.easy-kill
        epkgs.evil-nerd-commenter
        epkgs.expand-region
        epkgs.highlight-numbers
        epkgs.multiple-cursors
        epkgs.region-bindings-mode
        epkgs.shift-number
        epkgs.smartparens
        epkgs.string-inflection
        epkgs.undo-tree
        epkgs.wgrep
        epkgs.whole-line-or-region
        epkgs.ws-butler
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/edit.el ];
      ide.emacs.core.customKeymaps = {
        "custom-ws-map" = "C-c x";
        "misc-editing-map" = "<f5>";
        "token-editing-map" = "C-z";
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
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          # NOTE: expansions deps
          vim
        ];
        xdg.configFile."espanso/match/emacs-emergency.yml".source = yaml.generate "espanso-emacs-emergency.yml" {
          matches = [
            {
              trigger = ":ve";
              replace = "vim `rg --files | fzf`";
            }
          ];
        };
      };
    })
  ];
}
