(use-package sh-script
  :mode ("\\.sh$" . bash-ts-mode)
  :hook
  (bash-ts-mode-hook . lsp-deferred)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("bash-language-server" "start"))
                    :activation-fn (lsp-activate-on "shellscript")
                    :server-id 'bash-lsp)))
