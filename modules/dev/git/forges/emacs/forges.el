(use-package forge)

(use-package browse-at-remote
  :bind
  (:map custom-open-map
        ("r" . browse-at-remote)
        ("R" . browse-at-remote-kill))
  (:map magit-log-mode-map
        ("o" . browse-at-remote)
        ("y" . browse-at-remote-kill)))

(use-package git-link
  :bind
  (:map magit-status-mode-map
        ("o" . git-link)
        ("O" . git-link-commit))
  :custom
  (git-link-open-in-browser t)
  (git-link-use-commit t))
