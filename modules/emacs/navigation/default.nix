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
        type = types.enum [ "vertico" ];
        default = "vertico";
        description = "Selection UI to use, like Ivy, Vertico, etc.";
      };
      collections.backend = mkOption {
        type = types.enum [ "consult" ];
        default = "consult";
        description = "Collections completion backend to use, like Counsel, Consult, etc.";
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
      customWindowRecentering.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom windows recentering";
      };
      customWindowRecentering.eyeLevel = mkOption {
        type = types.float;
        default = 0.2;
        description = ''
          The relative position of the line considered as eye level in the
          current window, as a ratio between 0 and 1.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.ide.emacs.core.enable;
          message = "emacs/navigation: core configuration must be enabled.";
        }
        {
          assertion = config.ide.emacs.edit.enable;
          message = "emacs/navigation: emacs/edit must be enabled.";
        }
      ];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ ripgrep ];
      };
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.avy
        epkgs.ace-window
        epkgs.beginend
        epkgs.block-nav
        epkgs.bufler
        epkgs.burly
        epkgs.dired-filetype-face
        epkgs.dired-narrow
        epkgs.dired-subtree
        epkgs.embark
        epkgs.link-hint
        epkgs.manage-minor-mode-table
        epkgs.marginalia
        epkgs.mosey
        epkgs.orderless
        epkgs.pulsar
        epkgs.rainbow-delimiters
        epkgs.recursive-narrow
        epkgs.rg
        epkgs.winum
      ] ++ optionals (cfg.selection.backend == "vertico") [
        epkgs.vertico
      ] ++ optionals (cfg.collections.backend == "consult") [
        epkgs.consult
        epkgs.consult-dir
        epkgs.consult-flycheck
        epkgs.consult-project-extra
        epkgs.embark-consult
      ] ++ optionals (cfg.projects.backend == "project" && cfg.collections.backend == "consult") [
        epkgs.consult-project-extra
      ] ++ optionals (cfg.projects.backend == "projectile") [
        epkgs.projectile
        epkgs.flycheck-projectile
      ] ++ optionals (cfg.projects.backend == "projectile" && cfg.collections.backend == "consult") [
        epkgs.consult-projectile
      ];
      ide.emacs.core.customPackages = {
        "minibuffer-edit" = { text = builtins.readFile ./elisp/custom/minibuffer-edit.el; };
        "orderless-dispatchers" = { text = builtins.readFile ./elisp/custom/orderless-dispatchers.el; };
        "navigation-misc" = { text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/custom/misc.el ]; };
      } // optionalAttrs (cfg.collections.backend == "consult") {
        "consult-utils" = { text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/custom/consult-utils.el ]; };
      };
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ]
        ([ ./elisp/navigation.el ] ++ optionals (cfg.selection.backend == "vertico") [ ./elisp/vertico.el ]
          ++ optionals (cfg.collections.backend == "consult") [ ./elisp/consult.el ]
          ++ optionals (cfg.projects.backend == "project") [ ./elisp/project.el ]
          ++ optionals (cfg.projects.backend == "project" && cfg.collections.backend == "consult") [ ./elisp/consult-project.el ]
          ++ optionals (cfg.projects.backend == "projectile") [ ./elisp/projectile.el ]
          ++ optionals (cfg.projects.backend == "projectile" && cfg.collections.backend == "consult") [ ./elisp/consult-projectile.el ]);

      ide.emacs.core.customKeymaps = {
        "custom-help-map" = "<f1>";
        "custom-narrowing-map" = "<f9>";
        "custom-search-map" = "C-q";
        "custom-projects-map" = "<f11>";
        "custom-frame-map" = "<f2>";
      };
      # TODO: consider enhance custom keymaps nix machinery in a way of automatically unbinding, either global or some submapped conflicting keybindings
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        navigation_emacs = {
          filter_class = "Emacs";
          matches = [
            {
              trigger = ":rgr";
              replace = "[[elisp:(progn (require 'rg) (rg-run \"{{token.value}}\" \"everything\" default-directory nil nil '(\"--context={{context.size}}\")))]]";
              vars = [
                {
                  name = "token";
                  type = "form";
                  params = { layout = "search for: {{value}}"; };
                }
                {
                  name = "context";
                  type = "form";
                  params = { layout = "context size: {{size}}"; };
                }
              ];
            }
          ];
        };
      };
    })
  ];
}
