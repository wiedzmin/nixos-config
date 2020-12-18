(use-package git-timemachine
  :bind
  (:map mode-specific-map
        (";" . git-timemachine)))

(use-package git-walktree
  :after magit
  :bind
  (:map custom-magit-map
        ("o" . git-walktree)))

(use-package dired-git-info
  :after dired
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

;;TODO: ensure dependency (on clients nix submodule)
(use-package treemacs-magit
  :after treemacs magit)

(use-package magit-todos
  :after (magit projectile ivy)
  :bind
  (:map mode-specific-map
        ("C-d" . ivy-magit-todos))
  (:map custom-projectile-map
        ("t" . ivy-magit-todos)))
