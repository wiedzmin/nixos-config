(use-package autorevert
  :if (string-equal "i3" (getenv "CURRENT_WM"))
  :custom
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  :config
  (delight 'auto-revert-mode " ⟲" 'autorevert)
  (global-auto-revert-mode 1))
