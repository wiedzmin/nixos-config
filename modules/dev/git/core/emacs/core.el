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
  :after magit
  :preface
  (defun blamer-callback-show-commit-diff (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (magit-show-commit commit-hash))))
  (defun blamer-callback-open-remote (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (message commit-hash)
        (forge-browse-commit commit-hash))))
  (defun blamer-callback-magit-log-file (commit-info)
    (interactive)
    (magit-log-buffer-file)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (run-with-idle-timer 1 nil (lambda (commit-hash)
                                     (goto-char (point-min))
                                     (search-forward (substring commit-hash 0 7))
                                     (set-mark (point-at-bol))
                                     (goto-char (point-at-eol)))
                             commit-hash))))
  :bind
  (:map custom-git-map
        ("b" . blamer-mode)
        (">" . blamer-show-commit-info))
  :custom
  (blamer-idle-time 0.2)
  (blamer-min-offset 70)
  (blamer-view 'overlay)
  (blamer-author-formatter " ✎ %s ")
  (blamer-datetime-formatter " [%s] ")
  (blamer-commit-formatter "● %s")
  (blamer-prettify-time-p nil)
  (blamer-type 'both)
  (blamer-max-commit-message-length 100)
  (blamer-uncommitted-changes-message "Uncommitted")
  (blamer-smart-background-p nil)
  (blamer-bindings '(
                     ("<mouse-3>" . blamer-callback-open-remote)
                     ("<mouse-2>" . blamer-callback-magit-log-file)
                     ("<mouse-1>" . blamer-callback-show-commit-diff)))
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
