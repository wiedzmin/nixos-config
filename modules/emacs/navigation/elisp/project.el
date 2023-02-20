(use-package project
  :delight
  :preface
  (defun custom/project-project-name ()
    (file-name-nondirectory
     (directory-file-name
      (project-root (project-current)))))
  :bind
  (:map custom-projects-map
        ("b" . custom/kill-vc-current-buffer-file-path)
        ("d" . project-dired)
        ("r" . consult-recent-file)
        ("h" . consult-project-extra-find)
        ("k" . project-kill-buffers)))

(use-package consult-project-extra
  :bind
  ("C-<f1>" . consult-project-extra-find))

(with-eval-after-load 'lsp-mode
  (setq lsp-auto-guess-root t))
