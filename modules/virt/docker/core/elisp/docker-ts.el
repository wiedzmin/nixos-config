(use-package dockerfile-ts-mode
  :after lsp-mode
  :mode ("Dockerfile" . dockerfile-ts-mode)
  :hook
  (dockerfile-ts-mode-hook . lsp-deferred)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("docker-langserver" "--stdio"))
                    :activation-fn (lsp-activate-on "dockerfile")
                    :server-id 'dockerlsp)))
