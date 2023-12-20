(use-package flycheck-checkbashisms
  :hook (flycheck-mode-hook . flycheck-checkbashisms-setup))

(use-package pueue
  :bind
  (:map mode-specific-map
        ("q" . pueue)))

(with-eval-after-load 'sh-mode
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'company-tabnine)
    (add-to-list 'company-backends 'company-capf))
  (add-to-list 'completion-at-point-functions #'pcomplete-completions-at-point))
