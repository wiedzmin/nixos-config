(use-package browse-at-remote
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . browse-at-remote)
        ("R" . browse-at-remote-kill))
  (:map magit-log-mode-map
        ("o" . browse-at-remote)
        ("y" . browse-at-remote-kill)))


(use-package git-link
  :after link-hint
  :bind
  (:map magit-status-mode-map
        ("o" . git-link)
        ("O" . git-link-commit))
  :custom
  (git-link-open-in-browser t)
  (git-link-use-commit t))
