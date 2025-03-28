(use-package expand-region
  :bind
  ("C-," . er/expand-region)
  ("C-." . er/contract-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode))

(use-package hideshow
  :preface
  (defun custom/toggle-fold ()
    "Taken from: https://www.reddit.com/r/emacs/comments/746cd0/comment/dnwi2x1/"
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding)))
  :hook (prog-mode-hook . hs-minor-mode)
  :bind
  (:map mode-specific-map
        ("TAB" . custom/toggle-fold)))
