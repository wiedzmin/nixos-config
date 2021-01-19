(use-package go-mode
  :no-require t
  :mode ("\\.go$" . go-mode)
  :hook
  (before-save-hook . gofmt-before-save)
  (go-mode-hook . lsp-deferred)
  (go-mode-hook . whitespace-turn-off)
  :bind
  (:map go-mode-map
        ("C-k" . sp-kill-hybrid-sexp)
        ("C-<down>" . sp-push-hybrid-sexp)
        ("C-<right>" . sp-slurp-hybrid-sexp))
  :company company-tabnine
  :config
  (use-package lsp-go
    :custom
    (lsp-clients-go-server-args
     '("--cache-style=always"
       "--diagnostics-style=onsave"
       "--format-style=goimports")))
  ;;TODO: enable after proper setup
  (use-package dap-go
    :after lsp-mode dap-mode
    :disabled)
  (setq gofmt-command "goimports")
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.usePlaceholders" t t))))

(use-package flycheck-golangci-lint
  :after (flycheck go-mode)
  :hook (go-mode-hook . flycheck-golangci-lint-setup))

(use-package gotest
  :after (go-mode)
  :bind
  (:map go-mode-map
        ("C-c C-x f" . go-test-current-file)
        ("C-c C-x t" . go-test-current-test)
        ("C-c C-x p" . go-test-current-project)
        ("C-c C-x T" . go-test-current-benchmark)
        ("C-c C-x F" . go-test-current-file-benchmarks)
        ("C-c C-x P" . go-test-current-project-benchmarks)
        ("C-c C-x x" . go-run)))

(use-package go-tag
  :no-require t
  :after (go-mode)
  :bind
  (:map mode-specific-map
        :prefix-map custom-gotag-map
        :prefix "`"
        ("t" . go-tag-add)
        ("T" . go-tag-remove))
  :custom
  (go-tag-args '("-transform" "camelcase")))

(use-package ob-go
  :load-path "@emacsObGoPath@"
  :commands (org-babel-execute:go
             org-babel-expand-body:go
             org-babel-prep-session:go))
