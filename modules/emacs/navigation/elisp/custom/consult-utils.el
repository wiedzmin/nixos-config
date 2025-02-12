(defvar custom/consult-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-s" #'previous-history-element)
    map))

(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun consult-ripgrep-symbol-at-point ()
  (interactive)
  @projectBackendRequire@
  (consult-ripgrep @projectRootSexp@ (thing-at-point 'symbol)))
