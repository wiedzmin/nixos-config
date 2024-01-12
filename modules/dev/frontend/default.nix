{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = { home.packages = with pkgs; [ nodePackages.vue-language-server ]; };
      dev.editorconfig.rules = {
        "*.js" = { charset = "utf-8"; };
        "lib/**.js" = {
          indent_style = "space";
          indent_size = "2";
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.vue-mode ];
      ide.emacs.core.config = builtins.readFile ./elisp/frontend.el;
      ide.emacs.core.treesitter.grammars = {
        css = "https://github.com/tree-sitter/tree-sitter-css";
        html = "https://github.com/tree-sitter/tree-sitter-html";
        vue = "https://github.com/merico-dev/tree-sitter-vue";
        javascript = "https://github.com/tree-sitter/tree-sitter-javascript";
      };
      ide.emacs.core.treesitter.modeRemappings = {
        css-mode = "css-ts-mode";
        javascript-mode = "js-ts-mode";
        js-mode = "js-ts-mode";
        js2-mode = "js-ts-mode";
      };
    })
  ];
}
