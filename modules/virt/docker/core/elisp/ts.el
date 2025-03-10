(use-package dockerfile-ts-mode
  :mode ("Dockerfile" . dockerfile-ts-mode)
  :hook
  (dockerfile-ts-mode-hook . @lspStartFunction@)
  @lspModeDockerfileTSRegisterServer@)
