{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ide.emacs.edit;
  user = config.attributes.mainUser.name;
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
        epkgs.easy-kill-extras # add to .el
        epkgs.evil-nerd-commenter
        epkgs.expand-region
        epkgs.highlight-numbers
        epkgs.multiple-cursors
        epkgs.reformatter
        epkgs.region-bindings-mode
        epkgs.shift-number
        epkgs.smartparens
        epkgs.string-inflection
        epkgs.undo-tree
        epkgs.wgrep
        epkgs.whole-line-or-region
        epkgs.ws-butler
      ];
      home-manager.users."${user}" = {
        home.file = {
          ".emacs.d/resources/yasnippet" = {
            source = inputs.yasnippet-snippets;
            recursive = true;
          };
        };
      };
      ide.emacs.core.config = builtins.readFile ./elisp/edit.el;
      ide.emacs.core.customKeymaps = {
        "custom-ws-map" = "C-c x";
        "misc-editing-map" = "<f5>";
        "token-editing-map" = "C-z";
      };
    })
    (mkIf (cfg.enable && cfg.autorevert.enable) {
      ide.emacs.core.config = builtins.readFile ./elisp/autorevert.el;
    })
  ];
}
