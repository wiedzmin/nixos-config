(use-package go-mode
  :no-require t
  :mode ("\\.go$" . go-mode)
  :hook
  (before-save-hook . gofmt-before-save)
  (go-mode-hook . lsp-deferred)
  (go-mode-hook . whitespace-turn-off)
  (go-mode-hook . yas-minor-mode)
  :config
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'company-tabnine)
    (add-to-list 'company-backends 'company-capf))
  (add-to-list 'completion-at-point-functions #'pcomplete-completions-at-point)
  (use-package lsp-go)
  ;;TODO: enable after proper setup
  (use-package dap-go
    :after lsp-mode dap-mode
    :disabled)
  (setq gofmt-command "goimports"))

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
  (:map custom-gotag-map
        ("t" . go-tag-add)
        ("T" . go-tag-remove))
  :custom
  (go-tag-args '("-transform" "camelcase")))

(use-package ob-go
  :commands (org-babel-execute:go
             org-babel-expand-body:go
             org-babel-prep-session:go))
