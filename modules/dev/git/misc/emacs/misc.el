(use-package git-commit
  :after company-dabbrev
  :hook (text-mode-hook . company-mode)
  :company company-dabbrev)

(use-package git-msg-prefix
  :bind
  (:map git-commit-mode-map
        ("C-c i" . git-msg-prefix))
  :custom
  (git-msg-prefix-regex
   (rx bol (group (one-or-more
                   (group (zero-or-more alnum) ":" space)))))
  (git-msg-prefix-log-flags " --since='1 week ago' ")
  (git-msg-prefix-input-method 'completing-read))

(use-package smerge-mode
  :delight " ∓"
  :bind
  (:map mode-specific-map
        ("g k" . smerge-prev)
        ("g j" . smerge-next))
  :hook (find-file-hooks . (lambda ()
                             (save-excursion
                               (goto-char (point-min))
                               (when (re-search-forward "^<<<<<<< " nil t)
                                 (smerge-mode 1))))))

;TODO: investigate fringe updates lifecycle
(use-package diff-hl
  :preface
  (defun custom/toggle-diff-hl ()
    (interactive)
    (diff-hl-mode 0)
    (diff-hl-mode 1))
  :delight diff-hl-amend-mode
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (org-mode-hook . diff-hl-mode)
  (prog-mode . diff-hl-mode)
  :bind
  (:map mode-specific-map
        ("d" . custom/toggle-diff-hl))
  :config
  (when (string-equal "i3" (getenv "CURRENT_WM"))
    (diff-hl-margin-mode 1)
    (diff-hl-flydiff-mode 1)
    (global-diff-hl-mode 1))
  (diff-hl-amend-mode 1))
