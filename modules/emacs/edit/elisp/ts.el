(use-package treesit-fold
  :bind
  (:map mode-specific-map
        ("TAB" . treesit-fold-toggle))
  :config
  (global-treesit-fold-indicators-mode 1))
