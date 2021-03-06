{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

# TODO: review reference links:
# https://azzamsa.com/n/vue-emacs/
# https://emacs-lsp.github.io/lsp-mode/page/lsp-vetur/
# https://github.com/azzamsa/emacs.d/blob/master/modules/aza-lsp.el
# https://github.com/vuejs/vetur
# https://github.com/vuejs/vetur/tree/master/server
# https://medium.com/kloeckner-i/til-language-server-setup-for-elixir-and-vue-with-emacs-33a38be0672f
# https://www.google.com/search?num=100&q=emacs+vue+js+lsp
# https://www.reddit.com/r/emacs/comments/ciocbr/help_with_lsp_and_vue_language_server/
let
  cfg = config.dev.frontend;
  user = config.attributes.mainUser.name;
in {
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
    (mkIf (cfg.enable) {
      home-manager.users.${user} = { home.packages = with pkgs; [ nodePackages.vue-language-server ]; };
      dev.editorconfig.rules = {
        "*.js" = { charset = "utf-8"; };
        "lib/**.js" = {
          indent_style = "space";
          indent_size = "2";
        };
      };
      dev.projectenv.templates.entries = {
        "frontend.vue" = "${homePrefix (wsRoot "github")}/wiedzmin/nixos-config/modules/dev/frontend/templates/vue";
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.vue-mode ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/frontend.el;
    })
  ];
}
