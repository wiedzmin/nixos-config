(defun custom/ts/copy-function-name()
  (interactive)
  (let ((funcname
         (substring-no-properties
          (treesit-node-text
           (treesit-node-child-by-field-name (treesit-defun-at-point) "name")))))
    (kill-new funcname)
    (message "Copied name: %s" funcname)))
