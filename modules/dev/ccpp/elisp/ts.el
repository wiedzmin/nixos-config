(use-package lsp-clangd)

(use-package c-ts-mode
  :mode (("\\.c$" . c-ts-mode)
         ("\\.h$" . c-ts-mode))
  :hook
  ((c-ts-mode-hook c++-ts-mode-hook) . lsp-deferred)
  :config
  (setq-default c-basic-offset 2))
