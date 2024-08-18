(use-package c-mode
  :mode (("\\.c$" . c-mode)
         ("\\.h$" . c-mode))
  ;; :after dap
  :hook
  ((c-mode-hook c++-mode-hook) . @lspStartFunction@)
  @lspModeCRegisterServer@@eglotCRegisterServer@
  :config
  ;; (use-package dap-cpptools)
  (setq-default c-basic-offset 2))

(use-package c++-mode
  :mode (("\\.cpp$" . c++-mode)
         ("\\.h$" . c++-mode)
         ("\\.hpp$" . c++-mode))
  :hook
  ;; FIXME: consider making substitution as lsp server registration, parameterized with hook name(s)
  ((c-mode-hook c++-mode-hook) . @lspStartFunction@)
  @lspModeCPPRegisterServer@@eglotCPPRegisterServer@
  :config
  (setq-default c-basic-offset 2))

(use-package modern-cpp-font-lock
  :delight
  :config
  (modern-c++-font-lock-global-mode t))

(use-package cmake-mode
  :mode ("\\.cmake$" . cmake-mode)
  :hook
  (cmake-mode-hook . @lspStartFunction@)
  @eglotClientCmakeRegistration@)

(use-package cmake-font-lock)
