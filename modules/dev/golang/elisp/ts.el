(use-package go-ts-mode
  :no-require t
  :mode ("\\.go$" . go-ts-mode)
  :hook
  (before-save-hook . (lambda () (interactive) (when (eq major-mode 'go-ts-mode) (gofmt))))
  (go-ts-mode-hook . @lspStartFunction@)
  (go-ts-mode-hook . whitespace-turn-off)
  (go-ts-mode-hook . (lambda() (treesit-inspect-mode t)))
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (require 'go-mode) ;; NOTE: for "gofmt"
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
  :after (flycheck go-ts-mode)
  :hook (go-ts-mode-hook . flycheck-golangci-lint-setup))

(use-package gotest
  :after (go-ts-mode)
  :bind
  (:map go-ts-mode-map
        ("C-c C-x f" . go-test-current-file)
        ("C-c C-x t" . go-test-current-test)
        ("C-c C-x p" . go-test-current-project)
        ("C-c C-x T" . go-test-current-benchmark)
        ("C-c C-x F" . go-test-current-file-benchmarks)
        ("C-c C-x P" . go-test-current-project-benchmarks)
        ("C-c C-x x" . go-run)))

(use-package go-tag
  :no-require t
  :after (go-ts-mode)
  :bind
  (:map go-ts-mode-map
        ("C-c t" . go-tag-add)
        ("C-c T" . go-tag-remove))
  :custom
  (go-tag-args '("-transform" "camelcase")))

(with-eval-after-load 'eglot
  (with-eval-after-load 'go-ts-mode
    (setq-default eglot-workspace-configuration
                  '((:gopls .
                            ((staticcheck . t)
                             (matcher . "CaseSensitive")))))))
