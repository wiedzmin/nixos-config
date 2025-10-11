(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode))

;; TODO: try {ghr>org-annot-bridge<ghr}
(use-package pdf-tools
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
        ("/" . pdf-occur)
        (">" . pdf-view-goto-page)
        ("G" . pdf-view-last-page)
        ("g" . pdf-view-first-page)
        ("h" . image-backward-hscroll)
        ("i" . pdf-misc-display-metadata)
        ("j" . pdf-view-next-page)
        ("k" . pdf-view-previous-page)
        ("l" . image-forward-hscroll)
        ("u" . pdf-view-revert-buffer)
        ("y" . pdf-view-kill-ring-save))
  (:map pdf-annot-minor-mode-map
        ("a" . pdf-annot-attachment-dired)
        ("d" . pdf-annot-delete)
        ("l" . pdf-annot-list-annotations)
        ("m" . pdf-annot-add-markup-annotation)
        ("t" . pdf-annot-add-text-annotation))
  :hook
  (pdf-view-mode-hook . pdf-view-midnight-minor-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (pdf-loader-install)
  (pdf-annot-minor-mode 1))

;; FIXME: review/fix dependencies graph
(use-package org-noter
  :after (:any org pdf-tools)
  :commands org-noter
  :bind
  ;; TODO: bind #'org-noter in some org-related keymap
  ("C-c n o" . org-noter) ; FIXME: mode-specific-map
  ("C-c n q" . org-noter-kill-session)
  (:map org-noter-doc-mode-map
        ("C-M-," . org-noter-sync-current-page-or-chapter)
        ("C-M-." . org-noter-insert-precise-note)
        ("C-M-i" . org-noter-insert-note)
        ("C-M-n" . org-noter-sync-next-page-or-chapter)
        ("C-M-p" . org-noter-sync-prev-page-or-chapter)
        ("C-M-q" . org-noter-kill-session)
        ("C-M-s" . org-noter-create-skeleton)
        ("M-." . org-noter-sync-current-note)
        ("M-n" . org-noter-sync-next-note)
        ("M-p" . org-noter-sync-prev-note))
  (:map org-noter-notes-mode-map
        ("C-M-." . org-noter-insert-precise-note)
        ("C-M-i" . org-noter-insert-note)
        ("C-M-q" . org-noter-kill-session)
        ("C-M-s" . org-noter-create-skeleton)
        ("M-." . org-noter-sync-current-note)
        ("M-n" . org-noter-sync-next-note)
        ("M-p" . org-noter-sync-prev-note))
  (:map pdf-view-mode-map
        ("i" . org-noter))
  :custom
  (org-noter-arrow-delay 0.1)
  (org-noter-auto-save-last-location t)
  (org-noter-default-heading-title "Page $p$")
  (org-noter-default-notes-file-names '("Notes.org"))
  (org-noter-doc-property-in-notes t)
  (org-noter-doc-split-fraction '(0.67 . 0.67))
  (org-noter-highlight-selected-text t)
  (org-noter-max-short-selected-text-length 20)
  (org-noter-notes-search-path '("@orgNoterSearchPath@"))
  :config
  (require 'org-noter-pdftools))

(use-package org-pdftools
  :after org-noter
  :preface
  (defun custom/org-pdftools-get-desc (file page &optional text)
    (if text
        (concat "Page " page " quoting: " text)
      (concat "Page " page ": ")))
  :hook
  (org-load-hook . org-pdftools-setup-link)
  (org-store-link-functions . org-pdftools-store-link)
  :custom
  (org-pdftools-get-desc-function #'custom/org-pdftools-get-desc))

(use-package org-noter-pdftools
  :after org-noter
  :preface
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    "Ensure precise note is inserted"
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
     With a prefix ARG, remove start location.
     Fixes https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf"
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  :custom
  (org-noter-pdftools-use-org-id nil) ;; NOTE: Remove ID on PROPERTIES drawer
  (org-noter-pdftools-use-unique-org-id nil)
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
