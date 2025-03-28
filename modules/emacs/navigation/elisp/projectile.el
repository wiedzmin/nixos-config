(use-package projectile
  :delight
  :preface
  (defun projectile-switch-project-action-open-todos (project)
    "Open project's TODOs."
    (let ((projectile-switch-project-action #'custom/open-project-todos))
      (projectile-switch-project-by-name project)))
  :bind
  (:map custom-projects-map
        ("C" . projectile-commander)
        ("d" . projectile-dired)
        ("i" . projectile-invalidate-cache)
        ("k" . projectile-kill-buffers)
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
        ("!" . flycheck-projectile-list-errors)))

(with-eval-after-load 'marginalia
  (with-eval-after-load 'projectile
    (setq marginalia-command-categories '((projectile-find-file . project-file)
                                          (projectile-find-dir . project-file)
                                          (projectile-switch-project . file)))))

(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'projectile
    (setq lsp-auto-guess-root t)))
