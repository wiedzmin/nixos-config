(use-package projectile
  :delight
  :preface
  (defun custom/open-project-todos ()
    (interactive)
    (let ((todos-file (expand-file-name "todo.org" (projectile-project-root))))
      (condition-case nil
          (when (file-exists-p todos-file)
            (find-file todos-file))
        (error (message "Cannot find project todos")))))
  (defun custom/open-project-magit-status ()
    (interactive)
    (require 'anaphora)
    (let ((current-project-name (projectile-default-project-name (locate-dominating-file buffer-file-name ".git"))))
      (aif (get-buffer (concat "magit: " current-project-name))
          (switch-to-buffer it)
        (magit-status))))
  (defun projectile-switch-project-action-open-todos (project)
    "Open project's TODOs."
    (let ((projectile-switch-project-action #'custom/open-project-todos))
      (projectile-switch-project-by-name project)))
  (defun projectile-switch-project-action-open-magit-status (project)
    "Open project's Magit status buffer."
    (let ((projectile-switch-project-action #'custom/open-project-magit-status))
      (projectile-switch-project-by-name project)))
  :bind
  (:map custom-projects-map
        ("C" . projectile-commander)
        ("d" . projectile-dired)
        ("i" . projectile-invalidate-cache)
        ("k" . projectile-kill-buffers)
        ("T" . custom/open-project-todos)
        ("m" . custom/open-project-magit-status)
        ("f" . recentf-open-files)
        ("h" . projectile-find-file))
  :hook
  (after-init-hook . projectile-mode)
  :custom
  (projectile-completion-system 'default)
  (projectile-track-known-projects-automatically t)
  (projectile-enable-caching t)
  (projectile-require-project-root t)
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-project-search-path '(@projectsSearchPaths@))
  (projectile-project-root-functions
   '(projectile-root-local
     projectile-root-bottom-up))
  (projectile-project-root-files '(@projectsRootMarkersEmacs@)))

(use-package flycheck-projectile
  :after (projectile flycheck)
  :bind
  (:map mode-specific-map
        ("p" . flycheck-projectile-list-errors))
  (:map flycheck-mode-map
        ("C-c ! p" . flycheck-projectile-list-errors)))

(use-package treemacs-projectile
  :demand t
  :after treemacs projectile
  :bind
  (:map custom-projects-map
        ("e" . treemacs-projectile)))

(with-eval-after-load 'marginalia
  (setq marginalia-command-categories '((projectile-find-file . project-file)
                                        (projectile-find-dir . project-file)
                                        (projectile-switch-project . file))))

(with-eval-after-load 'lsp-mode
  (setq lsp-auto-guess-root t))
