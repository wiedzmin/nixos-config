(use-package autorevert
  :defer 2
  :custom
  (auto-revert-check-vc-info t)
  :config
  (global-auto-revert-mode 1))

(use-package backup-each-save
  :ensure t
  :hook (after-save-hook . backup-each-save))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package recentf
  :defer 1
  :config
  (use-package recentf-ext :ensure t)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 250)
  (recentf-max-menu-items 15))

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
          regexp-search-ring)))

(use-package savekill :ensure t)

(use-package saveplace
  :defer 1
  :config
  (save-place-mode 1))

(use-package super-save
  :ensure t
  :delight super-save-mode
  :custom
  (super-save-remote-files nil)
  :config
  (super-save-mode 1))

(setf create-lockfiles nil)
