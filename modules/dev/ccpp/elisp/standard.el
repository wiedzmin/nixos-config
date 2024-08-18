(use-package c-mode
  :after lsp-mode
  :mode (("\\.c$" . c-mode)
         ("\\.h$" . c-mode))
  ;; :after dap
  :hook
  ((c-mode-hook c++-mode-hook) . lsp-deferred)
  :config
  ;; (use-package dap-cpptools)
  (setq-default c-basic-offset 2))

(use-package c++-mode
  :after lsp-mode
  :mode (("\\.cpp$" . c++-mode)
         ("\\.h$" . c++-mode)
         ("\\.hpp$" . c++-mode))
  :hook
  ((c-mode-hook c++-mode-hook) . lsp-deferred)
  :config
  (setq-default c-basic-offset 2))

