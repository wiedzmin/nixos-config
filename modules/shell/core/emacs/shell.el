(use-package flycheck-checkbashisms
  :hook (flycheck-mode-hook . flycheck-checkbashisms-setup))

(use-package lsp-bash
  :ensure lsp-mode
  :hook
  (sh-mode-hook . lsp-deferred))
