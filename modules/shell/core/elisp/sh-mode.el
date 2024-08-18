(use-package sh-script
  :mode ("\\.sh$" . sh-mode)
  :hook
  (sh-mode-hook . @lspStartFunction@)
  @lspModeShellRegisterServer@)
