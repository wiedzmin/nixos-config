{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# TODO: add js/ts/whatever needed setups
# TODO: emmet + lsp: https://emacs-lsp.github.io/lsp-mode/page/lsp-emmet/ + https://github.com/smihica/emmet-mode
# TODO: elaborate setup for TypeScript
# TODO: investigate web-mode + lsp setup
# TODO: play with #''lsp-flycheck-add-mode for various major modes

let
  cfg = config.dev.frontend;
  user = config.attributes.mainUser.name;
in
{
  options = {
    dev.frontend = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable frontend development infrastructure.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
      emacs.orgmode.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs org-mode helper packages for org-babel and such";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          nodePackages.vue-language-server
          vscode-langservers-extracted
          typescript-language-server
        ];
      };
      dev.editorconfig.rules = {
        "*.js" = { charset = "utf-8"; };
        "lib/**.js" = {
          indent_style = "space";
          indent_size = "2";
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: optionals (!config.ide.emacs.core.treesitter.enable) [ epkgs.vue-mode ];
      ide.emacs.core.config = lib.optionalString (!config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/non-ts.el ]) +
        lib.optionalString (config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/ts.el ]) +
        lib.optionalString cfg.emacs.orgmode.enable ''
          (use-package ob-css
            :commands (org-babel-execute:css
                       org-babel-prep-session:css))

          (use-package ob-js
            :commands (org-babel-execute:js
                       org-babel-prep-session:js
                       org-babel-variable-assignments:js))
        '';
      ide.emacs.core.treesitter.grammars = {
        css = "https://github.com/tree-sitter/tree-sitter-css";
        html = "https://github.com/tree-sitter/tree-sitter-html";
        vue = "https://github.com/merico-dev/tree-sitter-vue";
        javascript = "https://github.com/tree-sitter/tree-sitter-javascript";
      };
      ide.emacs.core.treesitter.modeRemappings = {
        css-mode = "css-ts-mode";
        html-mode = "html-ts-mode";
        javascript-mode = "js-ts-mode";
        js-mode = "js-ts-mode";
        js2-mode = "js-ts-mode";
      };
    })
  ];
}
