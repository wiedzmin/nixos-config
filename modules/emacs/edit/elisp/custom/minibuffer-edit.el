(require 'simple)
(require 'thingatpt)

(defun minibuffer-edit--remove-until-slash (bound n)
  "Return the position of the backwards Nth slash until BOUND.
  If no slash was found, return BOUND."
  (save-excursion
    (if-let ((found (search-backward "/" bound 'noerror n)))
        (1+ found)
      bound)))

(defun minibuffer-edit-smart-delete-backwards ()
  "If `point' is at \"/\", delete till the last \"/\"."
  (interactive)
  (cond ((thing-at-point-looking-at "~/")
         (progn
           (delete-region (minibuffer-prompt-end) (point))
           (insert "/home/")))
        ((string= (buffer-substring (minibuffer-prompt-end) (point)) "/")
         (call-interactively #'backward-delete-char))
        ((thing-at-point-looking-at "/")
         (delete-region (minibuffer-edit--remove-until-slash (minibuffer-prompt-end) 2) (point)))
        (t (call-interactively #'backward-delete-char))))
