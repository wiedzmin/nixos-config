(use-package browse-at-remote
  :ensure t
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . browse-at-remote)
        ("k" . browse-at-remote-kill))
  (:map magit-status-mode-map
        ("o" . browse-at-remote))
  :custom
  (browse-at-remote-prefer-symbolic nil))

(use-package company-lsp
  :ensure t
  :after lsp-ui
  :custom
  (company-lsp-async t)
  (company-lsp-cache-candidates 'auto)
  (company-lsp-enable-recompletion t)
  (company-lsp-enable-snippet t)
  :config
  (push 'company-lsp company-backends))

(use-package company-nixos-options
  :ensure t
  :disabled
  :config
  (add-to-list 'company-backends 'company-nixos-options))

(use-package diff-hl
  :ensure t
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (org-mode-hook . diff-hl-mode)
  (prog-mode . diff-hl-mode)
  :config
  (diff-hl-margin-mode 1)
  (diff-hl-amend-mode 1)
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode 1))

(use-package diff-mode
  :mode "diff")

(use-package fic-mode
  :ensure t
  :hook
  (prog-mode . fic-mode))

(use-package git-timemachine
  :ensure t
  :bind
  (:map mode-specific-map
        (";" . git-timemachine)))

(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . company-mode)
  :custom
  (lsp-before-save-edits t)
  (lsp-eldoc-render-all nil)
  (lsp-highlight-symbol-at-point nil)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-prefer-flymake nil)
  :config
  (use-package lsp-clients))

(use-package lsp-ui
  :ensure t
  :after lsp-mode avy
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  (lsp-after-open-hook . lsp-enable-imenu)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  (:map mode-specific-map
        ("R" . lsp-restart-workspace))
  (:map custom-goto-map
        ("i" . lsp-ui-imenu))
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-update-mode 'point))

(use-package magit
  :ensure t
  :mode (("COMMIT_EDITMSG" . conf-javaprop-mode)
         ("COMMIT" . git-commit-mode))
  :bind
  (:prefix-map custom-magit-map
               :prefix "C-'"
               ("B" . magit-branch)
               ("L" . magit-reflog-current)
               ("O" . magit-reflog-other)
               ("R" . magit-rebase)
               ("S" . magit-stash)
               ("U" . magit-update-index)
               ("a" . magit-stage-file)
               ("b" . magit-blame-addition)
               ("c" . magit-checkout)
               ("d" . magit-diff)
               ("f" . magit-log-buffer-file)
               ("i" . magit-init)
               ("l" . magit-log)
               ("n" . magit-notes-edit)
               ("r" . magit-reset)
               ("s" . magit-status)
               ("t" . magit-tag)
               ("w" . magit-diff-working-tree))
  (:map magit-status-mode-map
        ("E" . nil)
        ("N" . magit-notes-edit)
        ("q" . custom/magit-kill-buffers))
  :preface
  (defun open-global-repos-list ()
    (interactive)
    (let ((repos-buffer (get-buffer "*Magit Repositories*")))
      (if repos-buffer
          (switch-to-buffer repos-buffer)
        (magit-list-repositories))))
  (defun custom/magit-restore-window-configuration (&optional kill-buffer)
    "Bury or kill the current buffer and restore previous window configuration."
    (let ((winconf magit-previous-window-configuration)
          (buffer (current-buffer))
          (frame (selected-frame)))
      (quit-window kill-buffer (selected-window))
      (when (and winconf (equal frame (window-configuration-frame winconf)))
        (set-window-configuration winconf)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq magit-previous-window-configuration nil))))))
  (defun custom/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  :secret "vcs.el.gpg"
  :custom
  (magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (magit-completing-read-function 'ivy-completing-read)
  (magit-blame-heading-format "%H %-20a %C %s")
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1))

(use-package magit-filenotify
  :ensure t
  :delight (magit-filenotify-mode " FN")
  :hook (magit-status-mode-hook . (lambda ()
                                    (condition-case nil
                                        (magit-filenotify-mode)
                                      (error (magit-filenotify-mode -1))))))

(use-package magit-todos
  :ensure t
  :hook
  (magit-status-mode . magit-todos-mode))

(use-package multi-compile :ensure t)

(use-package nix-mode
  :ensure t
  :mode (("\\.nix$" . nix-mode)
         ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-mode)))

(use-package smerge-mode
  :delight (smerge-mode "∓")
  :bind
  (:map mode-specific-map
        ("g k" . smerge-prev)
        ("g j" . smerge-next))
  :hook (find-file-hooks . (lambda ()
                             (save-excursion
                               (goto-char (point-min))
                               (when (re-search-forward "^<<<<<<< " nil t)
                                 (smerge-mode 1))))))

(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(dolist (mode '(paredit-mode smartparens-mode))
  (when (fboundp mode)
    (add-hook 'eval-expression-minibuffer-setup-hook mode)))

(use-package codesearch
  :ensure t
  :custom
  (codesearch-global-csearchindex "@emacsCodesearchIndex@"))

(use-package counsel-codesearch
  :ensure t
  :after codesearch
  :bind
  (:map mode-specific-map
        ("c" . counsel-codesearch)))

(use-package projectile-codesearch
  :ensure t
  :after codesearch)
