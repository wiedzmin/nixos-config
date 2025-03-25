(use-package recentf
  :after no-littering
  :config
  (use-package recentf-ext)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 250)
  (recentf-max-menu-items 15))

(use-package savekill)

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package files
  :hook
  (before-save-hook . delete-trailing-whitespace)
  :bind
  (:map ctl-x-map
        ("f" . find-file)
        ("g" . find-sibling-file))
  :custom
  (save-abbrevs 'silently)
  (view-read-only t)
  :config
  (setq safe-local-variable-values '((mode-require-final-newline))))

(use-package savehist
  :config
  (savehist-mode t)
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 60)
  (history-length 10000)
  (history-delete-duplicates t)
  (savehist-additional-variables
   '(kill-ring
     search-ring
     register-alist
     regexp-search-ring)))
