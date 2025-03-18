{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.lisp;
  user = config.attributes.mainUser.name;
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
      clojure.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Clojure setup";
      };
      emacs.orgmode.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs org-mode helper packages for org-babel and such";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.cl.enable {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.melpaStablePackages.slime
      ] ++ optionals (config.ide.emacs.completion.snippets.backend == "yasnippet") [
        epkgs.common-lisp-snippets
      ];
      ide.emacs.core.config = builtins.readFile ./elisp/cl.el +
        lib.optionalString cfg.emacs.orgmode.enable ''
          (use-package ob-lisp
            :commands (org-babel-execute:lisp
                       org-babel-expand-body:lisp))
        '';
    })
    (mkIf cfg.elisp.enable {
      ide.emacs.completion.tempel.snippets = ''
        org-mode

        (elisp "#+begin_src emacs-lisp" n> r> n "#+end_src" :post (org-edit-src-code))

        emacs-lisp-mode

        (deb "(progn (print \">>> " (s text) "\") (prin1 " text " t))" q)
      '';
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
      ide.emacs.core.treesitter.grammars = {
        elisp = "https://github.com/Wilfred/tree-sitter-elisp";
      };
    })
    (mkIf cfg.clojure.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ babashka ];
      };
    })
  ];
}
