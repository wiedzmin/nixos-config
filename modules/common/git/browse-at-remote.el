(use-package browse-at-remote
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . browse-at-remote)
        ("k" . browse-at-remote-kill))
  (:map magit-status-mode-map
        ("o" . browse-at-remote))
  :custom
  (browse-at-remote-prefer-symbolic nil))
