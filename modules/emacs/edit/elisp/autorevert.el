(use-package autorevert
  :custom
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  :config
  (delight 'auto-revert-mode " ⟲" 'autorevert)
  (global-auto-revert-mode 1))
