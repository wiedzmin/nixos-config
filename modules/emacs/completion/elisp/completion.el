(use-package dabbrev
  :custom
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))
