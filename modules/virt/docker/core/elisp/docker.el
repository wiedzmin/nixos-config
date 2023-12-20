(use-package dockerfile-mode
  :mode ("Dockerfile" . dockerfile-mode)
  :hook
  (dockerfile-mode-hook . lsp-deferred)
  :custom
  (dockerfile-mode-command "docker")
  :config
  (use-package lsp-dockerfile))
