(with-eval-after-load 'consult
  (defvar consult--source-org-buffer
    (list :name "Org"
          :narrow ?o
          :category 'buffer
          :state #'consult--buffer-state
          :items (lambda () (mapcar #'buffer-name (org-buffer-list)))))
  (add-to-list 'consult-buffer-sources consult--source-org-buffer 'append)
  (defun custom/consult-ripgrep-org ()
    (interactive)
    (consult-ripgrep "@orgRoot@"))
  (keymap-set custom-org-map "g" 'custom/consult-ripgrep-org))

(use-package consult-org-roam
  :preface
  (defun avy-action-search-org-roam (pt)
    "Search org-roam corpus for sexp at PT."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'sexp) ;TODO: fine-tune TAP type
        (let ((term (buffer-substring start end)))
          (message "searching `org-roam' data for: %s" term)
          (consult-org-roam-search term))))
    t)
  (defun avy-action-search-url-org-roam (pt)
    "Search org-roam corpus for sexp at PT."
    (save-excursion
      (goto-char pt)
      (condition-case nil
          (cl-destructuring-bind (start . end)
              (bounds-of-thing-at-point 'url)
            (let ((term (buffer-substring start end)))
              (message "searching `org-roam' data for: %s" term)
              (consult-org-roam-search term)))
        (error
         (message "%s seems to not being an URL" (thing-at-point 'sexp)))))
    t)
  :delight " |>"
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  (consult-org-roam-mode 1)
  :bind
  (:map org-roam-map
        ("b" . consult-org-roam-backlinks)
        ("g" . consult-org-roam-search))
  :config
  (setf (alist-get ?o avy-dispatch-alist) 'avy-action-search-org-roam
        (alist-get ?O avy-dispatch-alist) 'avy-action-search-url-org-roam))

;; [[file:~/workspace/repos/github.com/wiedzmin/nixos-config/modules/pim/orgmode/todo.org::*https://github.com/akirak/akirak-mode/blob/1fa4845e5ad4af95b58c3cdba7ead7223f05cef0/akirak-consult-org.el#L7][TODO: review custom solutions]]
