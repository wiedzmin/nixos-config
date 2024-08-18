(use-package c-ts-mode
  :mode (("\\.c$" . c-ts-mode)
         ("\\.h$" . c-ts-mode))
  :hook
  ((c-ts-mode-hook c++-ts-mode-hook) . @lspStartFunction@)
  @lspModeCTSRegisterServer@@eglotCTSRegisterServer@
  :config
  (setq-default c-basic-offset 2))

(use-package c++-ts-mode
  :mode (("\\.cpp$" . c++-ts-mode)
         ("\\.h$" . c++-ts-mode)
         ("\\.hpp$" . c++-ts-mode))
  :hook
  ((c-ts-mode-hook c++-ts-mode-hook) . @lspStartFunction@)
  @lspModeCPPTSRegisterServer@@eglotCPPTSRegisterServer@
  :config
  (setq-default c-basic-offset 2))

(use-package cmake-ts-mode
  :mode ("\\.cmake$" . cmake-ts-mode)
  :hook
  (cmake-ts-mode-hook . @lspStartFunction@)
  @eglotClientCmakeTSRegistration@)
