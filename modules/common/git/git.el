(use-package git-timemachine
  :bind
  (:map mode-specific-map
        (";" . git-timemachine)))

(use-package magit
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
               ("b" . magit-blame-addition) ; TODO: add for *-removal
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
        ("C-c k" . magit-process-kill)
        ("q" . custom/magit-kill-buffers))
  (:map dired-mode-map
        ("@" . magit-dired-log))
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
  :custom
  (magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (magit-completing-read-function 'ivy-completing-read)
  (magit-blame-heading-format "%H %-20a %C %s")
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1))

(use-package magit-filenotify
  :delight (magit-filenotify-mode " FN")
  :hook (magit-status-mode-hook . (lambda ()
                                    (condition-case nil
                                        (magit-filenotify-mode)
                                      (error (magit-filenotify-mode -1))))))

(use-package git-walktree
  :after magit
  :bind
  (:map custom-magit-map
        ("o" . git-walktree)))

(use-package dired-git-info
  :after dired
  ;; :hook (dired-after-readin-hook . dired-git-info-auto-enable)
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

(use-package git-msg-prefix
  :bind
  (:map git-commit-mode-map
        ("C-c i" . git-msg-prefix))
  :custom
  (git-msg-prefix-log-flags " --since='1 week ago' ")
  (git-msg-prefix-input-method 'ivy-read))

(use-package magit-todos
  :bind
  (:map mode-specific-map
        ("C-d" . ivy-magit-todos))
  :hook
  (magit-status-mode-hook . magit-todos-mode))

(use-package git-link
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . git-link)
        ("c" . git-link-commit))
  (:map magit-status-mode-map
        ("o" . git-link)
        ("H" . git-link-commit))
  :custom
  (git-link-open-in-browser t)
  (git-link-use-commit t))

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
