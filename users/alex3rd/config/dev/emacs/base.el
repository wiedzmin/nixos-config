(use-package amx
  :ensure t
  :bind
  ("M-x" . amx)
  :custom
  (amx-backend 'ivy)
  (amx-save-file "@emacsAmxSaveFile@"))

(use-package anaphora :ensure t)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

(use-package cus-edit
  :hook (kill-emacs-hook . (lambda () (delete-file custom-file)))
  :custom
  (custom-file "@emacsCustomFile@"))

(use-package delight :ensure t)

(use-package emacs
  :bind
  ("M-\"" . eval-region)
  ([remap kill-buffer] . kill-this-buffer)
  :secret "identity.el.gpg"
  :custom
  (use-dialog-box nil)
  (minibuffer-prompt-properties
        '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  (undo-limit 1000000)
  (indent-tabs-mode nil)
  (mark-even-if-inactive nil)
  (x-stretch-cursor t)
  (print-circle t)
  (print-gensym t)
  (sentence-end-double-space nil)
  (tab-always-indent t)
  (split-width-threshold nil)
  (split-height-threshold nil)
  (scroll-preserve-screen-position 'always)
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-charset-priority 'unicode)
  (set-default 'indent-tabs-mode nil) ;; Never insert tabs, !!!DO NOT REMOVE!!!
  (setq-default tab-width 4)
  (setq-default fill-column 200)
  (setq-default indicate-empty-lines t)
  (setq-default truncate-lines t))

(use-package executable
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package f
  :ensure t
  :after s dash)

(use-package files
  :hook
  (before-save-hook . delete-trailing-whitespace)
  :bind
  (:map ctl-x-map
        ("f" . find-file))
  :custom
  (require-final-newline t)
  (enable-local-variables nil)
  ;; backup settings
  (auto-save-default nil)
  (backup-by-copying t)
  (backup-by-copying-when-linked t)
  (backup-directory-alist '(("." . "~/.cache/emacs/backups")))
  (delete-old-versions -1)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (save-abbrevs 'silently))

(use-package gcmh
  :ensure t
  :delight " \ufe0f"
  :config
  (gcmh-mode 1))

(use-package iqa
  :ensure t
  :init
  (setq iqa-user-init-file "@emacsInitFile@")
  :config
  (iqa-setup-default))

(use-package jka-cmpr-hook
  :config
  (auto-compression-mode 1))

(use-package kmacro
  :custom
  (setq kmacro-ring-max 16))

(use-package no-littering
  :ensure t
  :custom
  (no-littering-var-directory "@emacsDataDir@/"))

(use-package novice
  :custom
  (disabled-command-function nil))

(use-package restart-emacs
  :ensure t
  :bind
  (:map ctl-x-map
        ("C-c" . restart-emacs)))

(use-package server
  :preface
  (defun custom/save-buffer-clients-on-exit ()
    (interactive)
    (if (and (boundp 'server-buffer-clients) server-buffer-clients)
        (server-save-edit)
      (save-buffers-kill-emacs t)))
  (defun custom/ensure-server ()
    (unless (and (string-equal "root" (getenv "USER"))
                 (server-running-p))
      (require 'server)
      (server-start)))
  :hook
  (after-init-hook . custom/ensure-server)
  :config
  (advice-add 'save-buffers-kill-terminal :before 'custom/save-buffer-clients-on-exit)
  (advice-add 'server-edit :before 'save-buffer))
