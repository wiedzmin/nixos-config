(use-package c-ts-mode
  :after lsp-mode
  :mode (("\\.c$" . c-ts-mode)
         ("\\.h$" . c-ts-mode))
  :hook
  ((c-ts-mode-hook c++-ts-mode-hook) . lsp-deferred)
  :config
  (setq-default c-basic-offset 2))

(use-package c++-ts-mode
  :after lsp-mode
  :mode (("\\.cpp$" . c++-ts-mode)
         ("\\.h$" . c++-ts-mode)
         ("\\.hpp$" . c++-ts-mode))
  :hook
  ((c-ts-mode-hook c++-ts-mode-hook) . lsp-deferred)
  :config
  (setq-default c-basic-offset 2))
