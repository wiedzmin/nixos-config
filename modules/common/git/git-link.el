(use-package git-link
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . git-link)
        ("c" . git-link-commit))
  (:map magit-status-mode-map
        ("o" . git-link))
  :custom
  (git-link-open-in-browser t))
