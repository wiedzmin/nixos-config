(use-package flycheck-checkbashisms
  :ensure t
  :hook (flycheck-mode-hook . flycheck-checkbashisms-setup))
