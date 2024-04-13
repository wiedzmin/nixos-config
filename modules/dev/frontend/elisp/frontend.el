(use-package vue-mode
  :disabled
  :mode "\\.vue\\'"
  :config
  ;FIXME: setup lsp
  (add-hook 'vue-mode-hook #'lsp-deferred))

(with-eval-after-load 'lsp-mode
  (mapc #'lsp-flycheck-add-mode '(typescript-mode js-mode css-mode vue-html-mode)))

;; TODO: add js/ts/whatever needed setups
;; TODO: emmet + lsp: https://emacs-lsp.github.io/lsp-mode/page/lsp-emmet/ + https://github.com/smihica/emmet-mode
