(use-package go-mode
  :no-require t
  :mode ("\\.go$" . go-mode)
  :hook
  (before-save-hook . gofmt-before-save)
  (go-mode-hook . lsp-deferred)
  (go-mode-hook . whitespace-turn-off)
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
