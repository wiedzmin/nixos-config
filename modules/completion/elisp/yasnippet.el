(use-package yasnippet
  :delight yas-minor-mode
  :mode (("@emacsYasnippetSnippets@" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode))
  :config
  (yas-global-mode)
  :custom
  (yas-key-syntaxes '("w" "w_" "w_." "^ " "w_.()" yas-try-key-from-whitespace))
  (yas-expand-only-for-last-commands '(self-insert-command))
  (yas-prompt-functions '(yas-completing-prompt
                          yas-x-prompt
                          yas-no-prompt))
  (yas-wrap-around-region t)
  (yas-snippet-dirs '("@emacsYasnippetSnippets@")))

(with-eval-after-load 'go-mode
  (add-hook 'consult-after-jump-hook 'yas-minor-mode))

(eval-after-load 'lsp
  (use-package lsp
    :custom
    (lsp-enable-snippet t)))
