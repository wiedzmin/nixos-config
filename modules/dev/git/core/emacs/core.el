(use-package git-modes ;TODO: consider adding more customizations
  :config
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

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
  (:map custom-git-map
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

(use-package blamer
  :bind
  (:map custom-git-map
        ("b" . blamer-mode))
  :custom
  (blamer-idle-time 0.2)
  (blamer-min-offset 70)
  (blamer-view 'overlay)
  (blamer-author-formatter " ✎ %s ")
  (blamer-datetime-formatter "[%s]")
  (blamer-commit-formatter "● %s")
  (blamer-prettify-time-p nil)
  (blamer-type 'both)
  (blamer-max-commit-message-length 100)
  (blamer-uncommitted-changes-message "Uncommitted")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :italic t))))

(use-package magit-commit-mark
  :after magit
  :commands (magit-commit-mark-mode)
  :bind
  (:map magit-log-mode-map
        (";" . magit-commit-mark-toggle-read)
        ("M-;" . magit-commit-mark-toggle-star)
        ("C-." . magit-commit-mark-toggle-urgent))
  :hook
  (magit-mode-hook . magit-commit-mark-mode)
  :custom
  (magit-commit-mark-on-show-commit nil)
  :custom-face
  (magit-commit-mark-read-face ((t :foreground "#7a88cf"
                                   :background nil
                                   :italic t))))
