(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))
