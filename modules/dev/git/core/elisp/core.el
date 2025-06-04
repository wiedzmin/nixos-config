(use-package git-modes
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
  (defun custom/magit-show-commit-by-rev (commit)
    (interactive "MCommit id: ")
    (magit-show-commit commit))
  (defun custom/magit-display-status-pop-up-frame (buffer)
    "Display `magit-status' in new frame when opening `orgit' links"
    (if (with-current-buffer buffer (eq major-mode 'magit-status-mode))
        (let* ((previous-buffer (caar (window-prev-buffers)))
               (previous-buffer-filename (buffer-file-name previous-buffer)))
          (if (and previous-buffer-filename
                   (string-suffix-p "\.org" previous-buffer-filename))
            (display-buffer buffer
                            '((display-buffer-reuse-window
                               display-buffer-pop-up-frame)
                              (reusable-frames . t)))
            (magit-display-buffer-same-window-except-diff-v1 buffer)))
      (magit-display-buffer-same-window-except-diff-v1 buffer)))
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
        ("c" . magit-checkout)
        ("w" . custom/magit-show-commit-by-rev))
  (:map magit-status-mode-map
        ("N" . magit-notes-edit)
        ("F" . magit-commit-instant-fixup)
        ("C-c k" . magit-process-kill))
  (:map dired-mode-map
        ("@" . magit-dired-log))
  :config
  (define-advice magit-whitespace-disallowed
      (:around (orig-fun &rest args) custom/magit-whitespace-to-underscore)
    (interactive)
    (insert "-"))
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
  (magit-display-buffer-function 'custom/magit-display-status-pop-up-frame)
  (magit-bury-buffer-function 'magit-restore-window-configuration))

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
                   :background "unspecified"
                   :italic t))))

(use-package magit-commit-mark
  :after magit
  :commands (magit-commit-mark-mode)
  :bind
  (:map magit-log-mode-map
        (";" . magit-commit-mark-toggle-read)
        ("M-;" . magit-commit-mark-toggle-star)
        ("C-." . magit-commit-mark-toggle-urgent)
        ("C-c M-;" . magit-commit-mark-report-star)
        ("C-c C-." . magit-commit-mark-report-urgent))
  :hook
  (magit-mode-hook . magit-commit-mark-mode)
  :custom
  (magit-commit-mark-on-show-commit nil)
  :custom-face
  (magit-commit-mark-read-face ((t :foreground "#7a88cf"
                                   :background "unspecified"
                                   :italic t))))

(use-package git-msg-prefix
  :load-path "@emacsGitMsgPrefixPath@"
  :bind
  (:map git-commit-mode-map
        ("C-c i" . git-msg-prefix))
  :custom
  (git-msg-prefix-regex
   (rx bol (group (one-or-more
                   (group (zero-or-more alnum) ":" space)))))
  (git-msg-prefix-log-flags " --since='1 week ago' ")
  (git-msg-prefix-input-method 'completing-read))

(with-eval-after-load 'hyperbole
  (defib ghq-path-tap ()
    "Create implicit button for opening repo path under `ghq' root dir.

It either opens `magit-status' or a [raw] git commit (if given), in case of git repo. Otherwise, it opens
`vc-dir' buffer for it."
    (let ((path (thing-at-point 'ghq-path)))
      (when path
        (ibut:label-set path)
        (hact 'custom/hypb/open-ghq-path-tap path))))

  (defun custom/hypb/open-ghq-path-tap (path)
    "Open `magit-status'/`vc-dir' or a [raw] git commit (if given) for given repository under `ghq' root dir."
    (let* ((colon-split (split-string path "\:"))
           (repo-path (nth 0 colon-split))
           (repo-name (nth 0 (last (split-string repo-path "\/"))))
           (default-directory (format "%s/%s" "@ghqRoot@" (nth 0 colon-split))))
      (if (magit-git-repo-p default-directory)
          (if (= (length colon-split) 2)
              (let* ((commit-hash (nth 1 colon-split))
                     (diff-buffer (generate-new-buffer (format "%s :: %s" repo-name commit-hash))))
                (with-current-buffer diff-buffer
                  (shell-command (format "git show %s" (nth 1 colon-split)) diff-buffer)
                  (diff-mode)
                  (read-only-mode 1))
                (display-buffer diff-buffer '(display-buffer-pop-up-window . nil))
                (select-window (get-buffer-window diff-buffer)))
            (magit-status default-directory))
        (vc-dir default-directory)))))
