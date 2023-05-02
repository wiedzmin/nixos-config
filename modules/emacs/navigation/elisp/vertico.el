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
        ("C-'" . vertico-quick-jump)))

(use-package vertico-multiform
  :after vertico
  :custom
  ;TODO: investigate options thoroughly
  (vertico-multiform-categories '((file buffer grid)
                                  (imenu (:not indexed mouse))
                                  (symbol (vertico-sort-function . vertico-sort-alpha))))
  :config
  (vertico-multiform-mode))
