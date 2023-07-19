(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-scroll-margin 0)
  (vertico-count @selectionCandidatesCount@)
  (vertico-resize t)
  (vertico-cycle t))

(use-package vertico-buffer
  :after vertico
  :init
  (vertico-buffer-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-quick
  :after vertico
  :bind
  (:map vertico-map
        ("C-." . vertico-quick-insert)
        ("C-'" . vertico-quick-jump))
  :custom
  (vertico-quick1 "qweasd")
  (vertico-quick2 "zxc"))

(use-package vertico-multiform
  :after vertico
  :custom
  ;TODO: investigate options thoroughly
  (vertico-multiform-categories '((file buffer grid)
                                  (buffer flat (vertico-cycle . t))
                                  (imenu (:not indexed mouse))
                                  (consult-grep buffer)
                                  (consult-xref buffer)
                                  (symbol (vertico-sort-function . vertico-sort-alpha))))
  (vertico-multiform-commands
      '((consult-imenu buffer indexed)
        (execute-extended-command unobtrusive)))
  :config
  (vertico-multiform-mode))
