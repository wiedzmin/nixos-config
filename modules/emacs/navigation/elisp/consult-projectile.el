(with-eval-after-load 'consult
  (setq consult-project-function #'projectile-project-root)
  (fset 'projectile-ripgrep 'consult-ripgrep))

(use-package consult-projectile
  :after projectile
  :bind
  ("C-<f1>" . consult-projectile))
