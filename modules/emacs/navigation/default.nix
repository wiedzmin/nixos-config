{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
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
      selection.backend = mkOption {
        type = types.enum [ "selectrum" "vertico" ];
        default = "vertico";
        description = "Selection UI to use, like Ivy, Selectrum, Vertico, etc.";
      };
      selection.candidatesCount = mkOption {
        type = types.int;
        default = 20;
        description = "Candidates count to show at once, for selection UI in use";
      };
      projects.backend = mkOption {
        type = types.enum [ "project" "projectile" ];
        default = "project";
        description = "Projects handling package to use. Currently, `project.el` and `projectile` are supported.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "emacs/navigation: core configuration must be enabled.";
      }];
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ ripgrep ];
      };
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.avy
        epkgs.beginend
        epkgs.block-nav
        epkgs.bookmark-view
        epkgs.bufler
        epkgs.burly
        epkgs.consult
        epkgs.consult-dir
        epkgs.consult-flycheck
        epkgs.consult-project-extra
        epkgs.dired-filetype-face
        epkgs.dired-narrow
        epkgs.dired-subtree
        epkgs.dogears
        epkgs.embark
        epkgs.embark-consult
        epkgs.fzf
        epkgs.goggles
        epkgs.imenu-anywhere
        epkgs.link-hint
        epkgs.manage-minor-mode-table
        epkgs.marginalia
        epkgs.mwim
        epkgs.orderless
        epkgs.phi-search
        epkgs.phi-search-mc
        epkgs.pulsar
        epkgs.rainbow-delimiters
        epkgs.recursive-narrow
        epkgs.rg
        epkgs.ripgrep
        epkgs.treemacs
        epkgs.treemacs-icons-dired
        epkgs.winum
        epkgs.zygospore
      ] ++ optionals (cfg.selection.backend == "selectrum") [
        epkgs.selectrum
      ] ++ optionals (cfg.selection.backend == "vertico") [
        epkgs.vertico
      ] ++ optionals (cfg.projects.backend == "project") [
        epkgs.consult-project-extra
      ] ++ optionals (cfg.projects.backend == "projectile") [
        epkgs.projectile
        epkgs.flycheck-projectile
        epkgs.consult-projectile
        epkgs.treemacs-projectile
      ];
      ide.emacs.core.customPackages = {
        "minibuffer-edit" = builtins.readFile ./elisp/custom/minibuffer-edit.el;
        "orderless-dispatchers" = builtins.readFile ./elisp/custom/orderless-dispatchers.el;
        "consult-utils" = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/custom/consult-utils.el ];
        "misc" = builtins.readFile ./elisp/custom/misc.el;
      };
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ]
        ([ ./elisp/navigation.el ] ++ optionals (cfg.selection.backend == "selectrum") [ ./elisp/selectrum.el ]
          ++ optionals (cfg.selection.backend == "vertico") [ ./elisp/vertico.el ]
          ++ optionals (cfg.projects.backend == "project") [ ./elisp/project.el ]
          ++ optionals (cfg.projects.backend == "projectile") [ ./elisp/projectile.el ]);

      ide.emacs.core.customKeymaps = {
        "custom-help-map" = "<f1>";
        "custom-narrowing-map" = "<f9>";
        "custom-search-map" = "C-q";
        "custom-projects-map" = "<f8>";
        "custom-treemacs-map" = "C-x t";
        "custom-ws-map" = "C-c x";
        "custom-goto-map" = "M-s";
        "custom-frame-map" = "<f2>";
      };
      # TODO: consider enhance custom keymaps nix machinery in a way of automatically unbinding, either global or some submapped conflicting keybindings
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/user/navigation_emacs.yml".text = ''
          name: navigation_emacs
          parent: default
          filter_class: "Emacs"

          matches:
            - trigger: ":rgr"
              replace: "[[elisp:(progn (require 'rg) (rg-run \"{{token.value}}\" \"everything\" default-directory nil nil '(\"--context={{context.size}}\")))]]"
              vars:
                - name: token
                  type: form
                  params:
                    layout: |
                      search for: {{value}}
                - name: context
                  type: form
                  params:
                    layout: |
                      context size: {{size}}
        '';
      };
    })
  ];
}
