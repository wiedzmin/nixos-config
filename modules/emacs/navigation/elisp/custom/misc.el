;; TODO: consider reorganizing
(require 'vc)
(require 's)

(defun custom/kill-vc-current-buffer-file-path ()
  (interactive)
  (let* ((current-file (buffer-file-name))
         (vcs-root (expand-file-name (vc-call-backend (vc-responsible-backend current-file) 'root current-file))))
    (kill-new (s-chop-prefix vcs-root current-file))))

(defcustom custom/recenter-window-eye-level @recenterWindowEyeLevel@
  "The relative position of the line considered as eye level in the
current window, as a ratio between 0 and 1.")

(defun custom/recenter-window ()
  "Scroll the window so that the current line is at eye level."
  (interactive)
  (let ((line (round (* (window-height) custom/recenter-window-eye-level))))
    (recenter line)))
