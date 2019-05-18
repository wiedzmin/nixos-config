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

(use-package backup-each-save
  :ensure t
  :hook (after-save-hook . backup-each-save))

(use-package super-save
  :ensure t
  :delight super-save-mode
  :custom
  (super-save-remote-files nil)
  :config
  (super-save-mode 1))

(use-package recentf
  :no-require t
  :defer 1
  :config
  (use-package recentf-ext :ensure t)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 250)
  (recentf-max-menu-items 15))

(setf create-lockfiles nil)

;;Make cursor stay in the same column when scrolling using pgup/dn.
;;Previously pgup/dn clobbers column position, moving it to the
;;beginning of the line.
;;<http://www.dotemacs.de/dotfiles/ElijahDaniel.emacs.html>
(defadvice custom/scroll-up (around ewd-scroll-up first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))
(defadvice custom/scroll-down (around ewd-scroll-down first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))

(use-package saveplace
  :defer 1
  :config
  (save-place-mode 1))

(use-package on-screen :ensure t)

(use-package filecache)

(use-package savekill :ensure t)

(use-package autorevert
  :defer 2
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-check-vc-info t)
  ;; (vc-handled-backends (delq 'Git vc-handled-backends))
  ;; (vc-handled-backends nil)
  :config
  (global-auto-revert-mode 1))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))
