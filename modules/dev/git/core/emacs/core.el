(use-package magit
  :preface
  (defun custom/magit-hide-devenv ()
    (interactive)
    (magit-with-toplevel
      (magit-call-git "hideenv")
      (magit-refresh)))
  (defun custom/magit-unhide-devenv ()
    (interactive)
    (magit-with-toplevel
      (magit-call-git "unhideenv")
      (magit-refresh)))
  (defun custom/magit-export-devenv ()
    (interactive)
    (magit-with-toplevel
      (magit-call-git "exportenv")
      (magit-refresh)))
  (defun custom/magit-remove-devenv ()
    (interactive)
    (magit-with-toplevel
      (magit-call-git "removeenv")
      (magit-refresh)))
  :mode (("COMMIT_EDITMSG" . conf-javaprop-mode)
         ("COMMIT" . git-commit-mode))
  :bind
  (:map custom-magit-map
        ("i" . magit-init)
        ("s" . magit-status)
        ("." . magit-dispatch)
        ("," . magit-file-dispatch)
        ("r" . magit-reflog-current)
        ("R" . magit-reflog-other)
        ("c" . magit-checkout))
  (:map magit-status-mode-map
        ("N" . magit-notes-edit)
        ("C-c k" . magit-process-kill))
  (:map dired-mode-map
        ("@" . magit-dired-log))
  :config
  (setq magit-completing-read-function
        (cond ((fboundp 'selectrum-mode) #'selectrum-completing-read)
              (t #'magit-builtin-completing-read)))
  (advice-add 'magit-whitespace-disallowed :around (lambda (orig-fun &rest args) (interactive) (insert "-")))
  (transient-append-suffix 'magit-stash "z" '("h" "Hide dev environment" custom/magit-hide-devenv))
  (transient-append-suffix 'magit-stash "h" '("u" "Unhide dev environment" custom/magit-unhide-devenv))
  (transient-append-suffix 'magit-stash "u" '("e" "Export dev environment" custom/magit-export-devenv))
  (transient-append-suffix 'magit-stash "e" '("r" "Remove dev environment" custom/magit-remove-devenv))
  :custom
  (magit-blame-heading-format "%H %-20a %C %s")
  (magit-section-visibility-indicator '("..." . t))
  (magit-completing-read-function 'completing-read)
  (magit-section-initial-visibility-alist '((stashes . hide) (untracked . hide) (unpushed . hide)))
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
