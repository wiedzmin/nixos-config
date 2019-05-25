(use-package gcmh
  :quelpa
  (gcmh :repo "koral/gcmh" :fetcher gitlab)
  :delight " \ufe0f"
  :config
  (gcmh-mode 1))

(use-package iqa
  :ensure t
  :init
  (setq iqa-user-init-file "@emacsInitFile@")
  :config
  (iqa-setup-default))

(use-package restart-emacs
  :ensure t
  :config
  (global-set-key (kbd "C-x C-c") 'restart-emacs))

(use-package bug-hunter :disabled)

(use-package no-littering
  :ensure t
  :custom
  (no-littering-var-directory "@emacsDataDir@/"))

(defvar enable-experimental-packages nil)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

(use-package f
  :ensure t
  :after (s dash))

(use-package anaphora :ensure t)

(use-package delight :ensure t)

(use-package emacs
  :general
  ("M-\"" 'eval-region)
  ([remap kill-buffer] 'kill-this-buffer)
  :secret "identity.el.gpg"
  :custom
  (scalable-fonts-allowed t)
  (use-dialog-box nil)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
        '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  (auto-window-vscroll nil)
  (undo-limit 1000000)
  (indent-tabs-mode nil)
  (mark-even-if-inactive nil)
  (x-stretch-cursor t)
  (print-circle t)
  (print-gensym t)
  (locale-coding-system 'utf-8)
  (sentence-end-double-space nil)
  (tab-always-indent t)
  (frame-inhibit-implied-resize nil)
  (split-width-threshold nil)
  (split-height-threshold nil)
  (same-window-buffer-names
        '("*Help*"))
  (scroll-preserve-screen-position 'always)
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; don't let the cursor go into minibuffer prompt
  (when (eq system-type 'gnu-linux)
    (setq x-alt-keysym 'meta))
  (set-default 'indent-tabs-mode nil);; Never insert tabs, !!!DO NOT REMOVE!!!
  (setq-default tab-width 4)
  (setq-default fill-column 200)
  (setq-default indicate-empty-lines t)
  (setq-default truncate-lines t)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'scroll-right 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ;; reduce point movement lag, see https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
  (advice-add 'undo-auto--last-boundary-amalgamating-number :override #'ignore) ;; https://stackoverflow.com/a/41560712/2112489
  (define-coding-system-alias 'UTF-8 'utf-8)
  (define-coding-system-alias 'utf-8-emacs 'utf-8) ; needed by bbdb...
  (define-coding-system-alias 'utf_8 'utf-8))

(use-package notifications :defer t)

(use-package alert
  :ensure t
  :commands alert
  :custom
  (alert-default-style 'libnotify))

(use-package cus-edit
  :hook (kill-emacs-hook . (lambda () (delete-file custom-file)))
  :custom
  (custom-file "@emacsCustomFile@"))

(use-package vlf
  :ensure t
  :after (ivy counsel)
  :config
  (ivy-add-actions 'counsel-find-file '(("l" vlf "view large file"))))

(use-package server
  :defer 2
  :preface
  (defun custom/server-save-edit ()
    (interactive)
    (save-buffer)
    (server-edit))
  (defun custom/save-buffer-clients-on-exit ()
    (interactive)
    (if (and (boundp 'server-buffer-clients) server-buffer-clients)
        (server-save-edit)
      (save-buffers-kill-emacs t)))
  :hook (server-visit-hook . (lambda () (local-set-key (kbd "C-c C-c") 'custom/server-save-edit)))
  :config
  (unless (and (string-equal "root" (getenv "USER"))
               (server-running-p))
    (require 'server)
    (server-start))
  (advice-add 'save-buffers-kill-terminal :before 'custom/save-buffer-clients-on-exit))

(use-package novice
  :custom
  (disabled-command-function nil))

(use-package files
  :hook
  (before-save-hook . delete-trailing-whitespace)
  :general
  ("C-x f" 'find-file) ; I never use set-fill-column and I hate hitting it by accident.
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

(use-package amx
  :ensure t
  :general ("M-x" 'amx)
  :custom
  (amx-backend 'ivy)
  (amx-save-file "@emacsAmxSaveFile@"))

(use-package paradox
  :ensure t
  :defer t
  :after (seq let-alist spinner)
  :secret "vcs.el.gpg"
  :commands paradox-list-packages
  :custom
  (paradox-execute-asynchronously t)
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  :config
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

(use-package executable
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package text-mode
  :hook (text-mode-hook . text-mode-hook-identify))

(use-package jka-cmpr-hook
  :config
  (auto-compression-mode 1))

(use-package kmacro
  :custom
  (setq kmacro-ring-max 16))
