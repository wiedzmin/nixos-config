(use-package org-project-capture
  :bind
  (:map custom-projects-map
        ("c" . org-project-capture-project-todo-completing-read))
  :config
  (setq org-project-capture-default-backend
        (make-instance 'org-project-capture-projectile-backend))
  (org-project-capture-per-project))

(use-package projects-misc
  :bind
  (:map custom-projects-map
        ("T" . custom/open-project-todos)))
