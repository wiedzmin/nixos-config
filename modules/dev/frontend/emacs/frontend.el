(use-package vue-mode
  :disabled
  :mode "\\.vue\\'"
  :config
  ;FIXME: setup lsp
  (add-hook 'vue-mode-hook #'lsp-deferred))

;TODO: add js/ts/whatever needed setups
