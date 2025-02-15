(with-eval-after-load 'consult
  (with-eval-after-load 'project
    (keymap-set custom-projects-map "h" 'consult-project-extra-find))
  (keymap-set custom-projects-map "r" 'consult-recent-file))

(use-package consult-project-extra
  :bind
  ("C-<f1>" . consult-project-extra-find))
