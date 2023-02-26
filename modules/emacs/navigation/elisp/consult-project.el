(with-eval-after-load 'consult
  (keymap-set custom-projects-map "r" 'consult-recent-file)
  (keymap-set custom-projects-map "h" 'consult-project-extra-find))

(use-package consult-project-extra
  :bind
  ("C-<f1>" . consult-project-extra-find))
