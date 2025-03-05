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

(use-package diff-hl
  :preface
  (defun custom/toggle-diff-hl ()
    (interactive)
    (if diff-hl-mode
        (diff-hl-mode 0)
      (progn
        (diff-hl-mode 1)
        (diff-hl-update))))
  :delight diff-hl-amend-mode
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (prog-mode-hook . turn-on-diff-hl-mode)
  (vc-dir-mode-hook . turn-on-diff-hl-mode)
  :bind
  (:map mode-specific-map
        ("d" . custom/toggle-diff-hl))
  :config
  (diff-hl-amend-mode 1))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package vc
  :bind
  (:map custom-vc-map
        ("s" . vc-dir)
        ("b" . vc-annotate)
        ("=" . vc-diff)
        ("d" . vc-diff)
        (", l" . vc-print-log))
  :hook (after-save-hook . vc-refresh-state)
  :custom
  (vc-handled-backends '(SVN Git Hg)))
