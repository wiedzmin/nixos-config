(use-package expand-region
  :bind
  ("C-," . er/expand-region)
  ("C-." . er/contract-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode))
