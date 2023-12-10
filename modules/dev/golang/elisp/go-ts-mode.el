(use-package go-ts-mode
  :no-require t
  :mode ("\\.go$" . go-ts-mode)
  :hook
  (before-save-hook . (lambda () (interactive) (when (eq major-mode 'go-ts-mode) (gofmt))))
  (go-ts-mode-hook . lsp-deferred)
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
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "gopls")
                    :activation-fn (lsp-activate-on "go")
                    :server-id 'gopls))
  (use-package lsp-go)
  ;;TODO: enable after proper setup
  (use-package dap-go
    :after lsp-mode dap-mode
    :disabled)
  (setq gofmt-command "goimports"))
