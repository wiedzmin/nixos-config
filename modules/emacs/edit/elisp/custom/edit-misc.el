(defun custom/delete-duplicate-words ()
  "Delete duplicate words via `query-replace-regexp'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (query-replace-regexp "\\(\\b\\w+\\b\\)\\W+\\1\\b" "\\1")))
