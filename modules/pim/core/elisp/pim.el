(use-package hyperbole
  :demand t
  :custom
  (hyrolo-file-list '("/home/alex3rd/docs/org/roam"))
  (hyrolo-highlight-face '(:background "SystemWindowText" :foreground "purple1" :underline t))
  (hsys-org-enable-smart-keys :buttons)
  :config
  (setq hbmap:dir-user "~/.hyperb")
  (remove-hook 'hyrolo-edit-hook #'hyrolo-set-date)
  (remove-hook 'hyrolo-add-hook #'hyrolo-set-date)
  (hyperbole-mode 1))

(with-eval-after-load 'magit
  (with-eval-after-load 'hyperbole
    (defib custom/hypb/open-ghq-repo-maybe-commit-tap-magit ()
      "Open `magit-status' or a commit (if given) for a repo under `ghq' root"
      (when-let ((regex "ghq#\\([a-z0-9\\:\\/\\.\\-]+\\)")
                 (path (save-excursion
                         (skip-chars-backward "a-z0-9-")
                         (looking-at regex)
                         (match-string-no-properties 1))))
        (ibut:label-set path
                        (match-beginning 1)
                        (match-end 1))
        (hact 'custom/hypb/open-ghq-tap path)))
    (defun custom/hypb/open-ghq-tap (path)
      "Open `magit-status' or a [raw] commit (if given) for a repo under `ghq' root"
      (let* ((colon-split (split-string path "\:"))
             (repo-path (nth 0 colon-split))
             (repo-name (last repo-path))
             (default-directory (format "%s/%s" "@ghqRoot@" (nth 0 colon-split))))
        (if (= (length colon-split) 2)
            (let* ((commit-hash (nth 1 colon-split))
                   (diff-buffer (generate-new-buffer (format "%s :: %s" repo-name commit-hash))))
              (with-current-buffer diff-buffer
                (shell-command (format "git show %s" (nth 1 colon-split)) diff-buffer)
                (diff-mode)
                (read-only-mode 1))
              (display-buffer diff-buffer '(display-buffer-pop-up-window . nil))
              (select-window (get-buffer-window diff-buffer)))
          (magit-status default-directory))))))
