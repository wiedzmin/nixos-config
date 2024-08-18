(use-package dockerfile-mode
  :mode ("Dockerfile" . dockerfile-mode)
  :hook
  (dockerfile-mode-hook . @lspStartFunction@)
  @lspModeDockerfileRegisterServer@
  :custom
  (dockerfile-mode-command "docker")
  :config
  (use-package lsp-dockerfile))
