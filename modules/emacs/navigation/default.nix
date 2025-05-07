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
        epkgs.bufler
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
        epkgs.recursive-narrow
        epkgs.rg
        epkgs.winnow
        epkgs.winum
      ] ++ optionals (cfg.selection.backend == "vertico") [
        epkgs.vertico
      ] ++ optionals (cfg.collections.backend == "consult") [
        epkgs.consult
        epkgs.consult-dir
        epkgs.consult-flycheck
        epkgs.consult-project-extra
        epkgs.embark-consult
      ] ++ optionals (cfg.projects.backend == "project") [
        epkgs.disproject
      ] ++ optionals (cfg.projects.backend == "project" && cfg.collections.backend == "consult") [
        epkgs.consult-project-extra
      ] ++ optionals (cfg.projects.backend == "projectile") [
        epkgs.projectile
        epkgs.flycheck-projectile
      ] ++ optionals (cfg.projects.backend == "projectile" && cfg.collections.backend == "consult") [
        epkgs.consult-projectile
      ];
      ide.emacs.core.customPackages = {
        "orderless-dispatchers" = { text = builtins.readFile ./elisp/custom/orderless-dispatchers.el; };
        "navigation-misc" = { text = builtins.readFile ./elisp/custom/misc.el; };
      } // optionalAttrs (cfg.collections.backend == "consult") {
        "consult-utils" = { text = readSubstituted config inputs pkgs [ ./subst/consult-utils.nix ] [ ./elisp/custom/consult-utils.el ]; };
      };
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst/navigation.nix ] [ ./elisp/navigation.el ] +
        (optionalString (cfg.selection.backend == "vertico") (readSubstituted config inputs pkgs [ ./subst/vertico.nix ] [ ./elisp/vertico.el ])) +
        (optionalString (cfg.collections.backend == "consult") (readSubstituted config inputs pkgs [ ./subst/consult.nix ] [ ./elisp/consult.el ])) +
        (optionalString (cfg.projects.backend == "project") (readSubstituted config inputs pkgs [ ./subst/project.nix ] [ ./elisp/project.el ])) +
        (optionalString (cfg.projects.backend == "project" && cfg.collections.backend == "consult") (readSubstituted config inputs pkgs [ ./subst/consult-project.nix ] [ ./elisp/consult-project.el ])) +
        (optionalString (cfg.projects.backend == "projectile") (readSubstituted config inputs pkgs [ ./subst/projectile.nix ] [ ./elisp/projectile.el ])) +
        (optionalString (cfg.projects.backend == "projectile" && cfg.collections.backend == "consult") (builtins.readFile ./elisp/consult-projectile.el));

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
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "emacs/vocem/consult" = {
          desc = "VOCEM consult repo";
          remote.url = "https://github.com/minad/consult";
        };
        "emacs/vocem/consult/wiki" = {
          desc = "VOCEM consult wiki";
          remote.url = "https://github.com/minad/consult/wiki";
        };
        "emacs/vocem/vertico" = {
          desc = "VOCEM vertico repo";
          remote.url = "https://github.com/minad/vertico";
        };
        "emacs/vocem/vertico/wiki" = {
          desc = "VOCEM vertico wiki";
          remote.url = "https://github.com/minad/vertico/wiki";
        };
        "emacs/vocem/embark" = {
          desc = "VOCEM embark repo";
          remote.url = "https://github.com/oantolin/embark";
        };
        "emacs/vocem/embark/wiki" = {
          desc = "VOCEM embark wiki";
          remote.url = "https://github.com/oantolin/embark/wiki";
        };
        "emacs/vocem/orderless" = {
          desc = "VOCEM orderless repo";
          remote.url = "https://github.com/oantolin/orderless";
        };
        "emacs/corfu" = {
          desc = "corfu repo";
          remote.url = "https://github.com/minad/corfu";
        };
        "emacs/corfu/wiki" = {
          desc = "corfu/wiki";
          remote.url = "https://github.com/minad/corfu/wiki";
        };
        "emacs/cape" = {
          desc = "cape repo (corfu extension)";
          remote.url = "https://github.com/minad/cape";
        };
      };
    })
  ];
}
