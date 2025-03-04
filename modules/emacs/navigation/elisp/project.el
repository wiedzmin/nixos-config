(use-package project
  :delight
  :preface
  (defun custom/project-project-name ()
    (file-name-nondirectory
     (directory-file-name
      (project-root (project-current)))))
  :bind
  ("C-<f1>" . project-switch-project)
  (:map custom-projects-map
        ("b" . custom/kill-vc-current-buffer-file-path)
        ("d" . project-dired)
        ("k" . project-kill-buffers)))

(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'project
    (setq lsp-auto-guess-root t)))
