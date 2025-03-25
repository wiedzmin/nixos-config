(use-package backup-each-save
  :hook (after-save-hook . backup-each-save)
  :config
  (setq backup-each-save-mirror-location "@backupsRoot@"))

(use-package files
  :custom
  (auto-save-default nil)
  (backup-by-copying t)
  (backup-by-copying-when-linked t)
  (backup-directory-alist '(("." . "~/.cache/emacs/backups")))
  (delete-old-versions -1)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

(use-package backups-misc
  :bind
  (:map mode-specific-map
      ("b" . custom/open-buffer-file-backups-dir)))
