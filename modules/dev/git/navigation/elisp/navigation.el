(use-package git-timemachine
  :bind
  (:map mode-specific-map
        (";" . git-timemachine)))

(use-package git-walktree
  :after magit
  :bind
  (:map custom-git-map
        ("o" . git-walktree)))

(use-package dired-git-info
  :after dired
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

(use-package magit-todos
  :after magit
  :bind
  (:map mode-specific-map
        ("C-d" . magit-todos-list))
  (:map custom-projects-map
        ("t" . magit-todos-list))
  :config
  (magit-todos-mode 1))
