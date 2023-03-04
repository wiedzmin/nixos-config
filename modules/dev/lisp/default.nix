{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.lisp;
in
{
  options = {
    dev.lisp = {
      cl.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Common Lisp setup";
      };
      elisp.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs Lisp setup";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.cl.enable {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.melpaStablePackages.slime
      ] ++ optionals (cfg.emacs.backend == "yasnippet") [
        epkgs.common-lisp-snippets
      ];
      ide.emacs.core.config = builtins.readFile ./elisp/cl.el;
    })
    (mkIf cfg.elisp.enable {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.elsa
        epkgs.erefactor
        epkgs.eros
        epkgs.flycheck-elsa
        epkgs.flycheck-package
        epkgs.highlight-defined
        epkgs.highlight-quoted
        epkgs.ipretty
        epkgs.suggest
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/elisp.el ];
    })
  ];
}
