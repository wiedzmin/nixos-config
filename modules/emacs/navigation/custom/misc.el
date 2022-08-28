;; TODO: consider reorganizing
(require 'vc)
(require 's)

(defun custom/kill-vc-current-buffer-file-path ()
  (interactive)
  (let* ((current-file (buffer-file-name))
         (vcs-root (expand-file-name (vc-call-backend (vc-responsible-backend current-file) 'root current-file))))
    (kill-new (s-chop-prefix vcs-root current-file))))
