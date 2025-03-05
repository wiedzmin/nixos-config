(with-eval-after-load 'consult
  (with-eval-after-load 'projectile
    (setq consult-project-function #'projectile-project-root)
    (fset 'projectile-ripgrep 'consult-ripgrep)))

(use-package consult-projectile
  :after projectile
  :bind
  ("C-<f1>" . consult-projectile-switch-project)
  (:map custom-projects-map
        ("h" . consult-projectile)))
