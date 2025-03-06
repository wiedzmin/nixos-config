;; TODO: consider reorganizing
(require 'vc)
(require 's)

(defun custom/kill-vc-current-buffer-file-path (&optional absolute)
  (interactive "P")
  (let* ((current-file (buffer-file-name))
         (vcs-root (expand-file-name (vc-call-backend (vc-responsible-backend current-file) 'root current-file))))
    (kill-new (if absolute
                  current-file
                (s-chop-prefix vcs-root current-file)))))

(defun custom/dired-open-in-eww ()
  (interactive)
  (when (derived-mode-p 'dired-mode)
    (if (file-directory-p (dired-get-filename))
        (message "Directories cannot be opened in EWW")
      (eww-open-file (dired-get-file-for-visit)))))

(defun custom/dired-current-path-to-clipboard ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (kill-new dired-directory)))
