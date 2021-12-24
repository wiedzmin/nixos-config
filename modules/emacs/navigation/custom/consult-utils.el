(defvar my-consult-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-s" #'previous-history-element)
    map))

(defun define-minibuffer-key (key &rest defs)
  "Define KEY conditionally in the minibuffer.
  DEFS is a plist associating completion categories to commands."
  (define-key minibuffer-local-map key
              (list 'menu-item nil defs :filter
                    (lambda (d)
                      (plist-get d (completion-metadata-get
                                    (completion-metadata (minibuffer-contents)
                                                         minibuffer-completion-table
                                                         minibuffer-completion-predicate)
                                    'category))))))

(defun consult-find-for-minibuffer ()
  "Search file with find, enter the result in the minibuffer."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (default-directory (file-name-directory (minibuffer-contents)))
         (file (consult--find
                (replace-regexp-in-string
                 "\\s-*[:([].*"
                 (format " (via find in %s): " default-directory)
                 (minibuffer-prompt))
                #'consult--find-builder
                (file-name-nondirectory (minibuffer-contents)))))
    (delete-minibuffer-contents)
    (insert (expand-file-name file default-directory))
    (exit-minibuffer)))

(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun consult-ripgrep-symbol-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))

;;TODO: add #'consult-focus-lines-symbol-at-point for occasssional reference write-ups
(defun custom/embark-preview ()
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action))
        (embark-default-action)))))
