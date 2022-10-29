(use-package dockerfile-mode
  :mode ("Dockerfile" . dockerfile-mode)
  :hook
  (dockerfile-mode-hook . lsp-deferred)
  :custom
  (dockerfile-mode-command "docker")
  :config
  (put 'docker-image-name 'safe-local-variable #'stringp))
