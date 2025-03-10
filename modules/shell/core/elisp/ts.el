(use-package sh-script
  :mode ("\\.sh$" . bash-ts-mode)
  :hook
  (bash-ts-mode-hook . @lspStartFunction@)
  @lspModeShellTSRegisterServer@)
