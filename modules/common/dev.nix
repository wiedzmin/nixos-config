{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.dev;
  emacsDevSetup = ''
    (use-package webpaste
      :ensure t
      :bind
      (:prefix-map custom-webpaste-map
                   :prefix "M-p"
                   ("b" . webpaste-paste-buffer)
                   ("r" . webpaste-paste-region))
      :custom
      (webpaste-provider-priority '("ix.io" "gist.github.com")))

    (use-package jinja2-mode
      :ensure t
      :mode "\\.j2$")

    (use-package yaml-mode
      :mode "\\.yml\\'"
      :quelpa
      (yaml-mode :repo "yoshiki/yaml-mode" :fetcher github :version original))

    (use-package diff-hl
      :ensure t
      :hook
      (dired-mode-hook . diff-hl-dired-mode)
      (magit-post-refresh-hook . diff-hl-magit-post-refresh)
      (org-mode-hook . diff-hl-mode)
      (prog-mode . diff-hl-mode)
      :config
      (diff-hl-margin-mode 1)
      (diff-hl-amend-mode 1)
      (diff-hl-flydiff-mode 1)
      (global-diff-hl-mode 1))

    (use-package diff-mode
      :mode "diff")

    (use-package fic-mode
      :ensure t
      :hook
      (prog-mode . fic-mode))

    (use-package multi-compile :ensure t)

    (use-package company-restclient
      :ensure t
      :after company restclient
      :config
      (add-to-list 'company-backends 'company-restclient))

    (use-package company-lsp
      :ensure t
      :after lsp-ui
      :custom
      (company-lsp-async t)
      (company-lsp-cache-candidates 'auto)
      (company-lsp-enable-recompletion t)
      (company-lsp-enable-snippet t)
      :config
      (push 'company-lsp company-backends))

    (use-package lsp-mode
      :ensure t
      :hook (lsp-mode . company-mode)
      :custom
      (lsp-before-save-edits t)
      (lsp-eldoc-render-all nil)
      (lsp-highlight-symbol-at-point nil)
      (lsp-inhibit-message t)
      (lsp-message-project-root-warning t)
      (lsp-prefer-flymake nil)
      :config
      (use-package lsp-clients))

    (use-package lsp-ui
      :ensure t
      :after lsp-mode avy
      :hook
      (lsp-mode-hook . lsp-ui-mode)
      (lsp-after-open-hook . lsp-enable-imenu)
      :bind
      (:map lsp-ui-mode-map
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references))
      (:map mode-specific-map
            ("R" . lsp-restart-workspace))
      (:map custom-goto-map
            ("i" . lsp-ui-imenu))
      :custom
      (lsp-ui-sideline-enable t)
      (lsp-ui-sideline-ignore-duplicate t)
      (lsp-ui-sideline-show-code-actions t)
      (lsp-ui-sideline-show-hover t)
      (lsp-ui-sideline-show-symbol t)
      (lsp-ui-sideline-update-mode 'point))
  '';
in {
  options = {
    custom.dev = {
      playground.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable non-production tools to play with.";
      };
      patching.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable patching helper tools.";
      };
      statistics.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable code stats tools.";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable various misc tools.'';
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable development infra for Emacs.'';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.playground.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          # https://github.com/Matty9191/ssl-cert-check
          # https://github.com/alexmavr/swarm-nbt
          # https://github.com/moncho/dry
          # https://hub.docker.com/r/nicolaka/netshoot/
          # rstudio # qt plugins broken
          drone
          drone-cli
          jenkins
          python3Packages.deprecated
          python3Packages.unittest-data-provider
          terracognita
          terraform
          tflint
        ];
      };
    })
    (mkIf cfg.patching.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          patchutils
          wiggle
        ];
      };
    })
    (mkIf cfg.statistics.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          cloc
          gource
          sloccount
          tokei
        ];
      };
    })
    (mkIf cfg.misc.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          watchexec
          wstunnel
          pv
          loop
          ix
        ];
      };
    })
    (mkIf cfg.emacs.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [
          epkgs.company-restclient
          epkgs.diff-hl
          epkgs.fic-mode
          epkgs.jinja2-mode
          epkgs.multi-compile
          epkgs.webpaste
        ];
      };
      ide.emacs.config = ''${emacsDevSetup}'';
    })
  ];
}
