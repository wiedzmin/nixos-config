(defun custom/open-buffer-file-backups-dir (arg)
  (interactive "P")
  (when buffer-file-name
    (let* ((fn-dir (string-trim-right default-directory "/"))
           (fn-backup-dir (concat "@backupsRoot@" fn-dir))) ; note trailing slash absence
      (when (file-exists-p fn-backup-dir)
        (if current-prefix-arg
            (find-file-other-frame fn-backup-dir)
          (find-file fn-backup-dir))))))
