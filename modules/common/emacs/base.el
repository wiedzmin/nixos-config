(setq load-prefer-newer t)
(setq message-log-max t) ;; we don't want to lose any startup log info
(setq shell-file-name "@bashBinary@")

(setq gc-cons-percentage 0.3)

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 800000)))

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))

(add-hook 'focus-out-hook #'garbage-collect)

(require 'cl)
(require 'package)

(unless package--initialized
  (package-initialize))
(setq package-enable-at-startup nil)

(mapcar
 (lambda (package)
   (unless (package-installed-p package)
     (unless package-archive-contents
       (package-refresh-contents))
     (package-install package)))
 '(use-package pinentry))

(setq use-package-compute-statistics t)
(setq use-package-verbose t)
(setq use-package-hook-name-suffix "")
(put 'use-package 'lisp-indent-function 1)

(use-package quelpa :defer t)
(use-package quelpa-use-package
  :custom
  (quelpa-use-package-inhibit-loading-quelpa
   t "Improve startup performance"))
(use-package anaphora)

(use-package use-package-custom-update
  :quelpa
  (use-package-custom-update
   :repo "a13/use-package-custom-update" :fetcher github :version original))

(pinentry-start)

(use-package deferred)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(global-set-key (kbd "C-x C-.") ;; Run use-package linter
                (lambda ()
                  (interactive)
                  (find-file (concat user-emacs-directory "init.el"))
                  (use-package-lint)))
(global-set-key (kbd "C-x C-,") #'goto-char)

(use-package amx
  :bind
  ("M-x" . amx)
  :custom
  (amx-backend 'ivy)
  (amx-save-file "@emacsDatadir@/amx-items"))

(use-package auto-compile
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

(use-package delight)

(use-package emacs
  :bind
  ("M-\"" . eval-region)
  ([remap kill-buffer] . kill-this-buffer)
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
  :after s dash)

(use-package files
  :hook
  (before-save-hook . delete-trailing-whitespace)
  :bind
  (:map ctl-x-map
        ("f" . find-file))
  :custom
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
  :delight " \ufe0f"
  :config
  (gcmh-mode 1))

(use-package iqa
  :config
  (iqa-setup-default))

(use-package jka-cmpr-hook
  :config
  (auto-compression-mode 1))

(use-package kmacro
  :custom
  (setq kmacro-ring-max 16))

(use-package no-littering
  :custom
  (no-littering-var-directory "@emacsDatadir@/"))

(use-package novice
  :custom
  (disabled-command-function nil))

(use-package restart-emacs
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

(imagemagick-register-types)

(use-package autorevert
  :defer 2
  :custom
  (auto-revert-check-vc-info t)
  :config
  (global-auto-revert-mode 1))

(use-package backup-each-save
  :hook (after-save-hook . backup-each-save))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package recentf
  :defer 1
  :config
  (use-package recentf-ext)
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

(use-package savekill)

(use-package saveplace
  :defer 1
  :config
  (save-place-mode 1))

(use-package super-save
  :delight super-save-mode
  :custom
  (super-save-remote-files nil)
  :config
  (super-save-mode 1))

(setf create-lockfiles nil)

(use-package beginend
  :delight beginend-global-mode beginend-prog-mode beginend-magit-status-mode
  :config
  (beginend-global-mode))

(use-package comment-dwim-2
  :bind
  (:map mode-specific-map
        ("]" . comment-dwim-2))
  :custom
  (cd2/region-command 'cd2/comment-or-uncomment-region))

(use-package company
  :delight (company-mode " γ")
  :hook (after-init-hook . global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("M-." . company-show-location)
        ("C-c h" . company-quickhelp-manual-begin))
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  :config
  (use-package company-fuzzy
    :custom
    (company-fuzzy-prefix-ontop t)
    (company-fuzzy-show-annotation t)
    (company-fuzzy-sorting-backend 'alphabetic)
    :config
    (add-to-list 'company-fuzzy--no-prefix-backends 'company-yasnippet)
    (global-company-fuzzy-mode 1))
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode 1))
  (use-package company-statistics
    :config
    (company-statistics-mode)))

(use-package copy-as-format
  :bind
  (:map mode-specific-map
        :prefix-map custom-formatting-map
        :prefix "f"
        ("s" . copy-as-format-slack)
        ("g" . copy-as-format-github)
        ("o" . copy-as-format-org-mode)
        ("m" . copy-as-format-markdown)
        ("a" . copy-as-format-asciidoc)
        ("b" . copy-as-format-bitbucket)
        ("d" . copy-as-format-disqus)
        ("l" . copy-as-format-gitlab)
        ("c" . copy-as-format-hipchat)
        ("h" . copy-as-format-html)
        ("j" . copy-as-format-jira)
        ("w" . copy-as-format-mediawiki)
        ("p" . copy-as-format-pod)
        ("r" . copy-as-format-rst)
        ("f" . copy-as-format)))

(use-package delsel
  :bind
  (:map mode-specific-map
        ("C-g" . minibuffer-keyboard-quit))
  :config
  (delete-selection-mode 1))

(use-package goto-char-preview
  :bind
  ([remap goto-char] . goto-char-preview))

(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

(use-package editorconfig
  :delight (editorconfig-mode " EC")
  :hook ((prog-mode-hook text-mode-hook) . editorconfig-mode))

(use-package flycheck
  :preface
  ;; CREDITS: https://github.com/nathankot/dotemacs
  (defvar counsel-flycheck-history nil
    "History for `counsel-flycheck'")
  (defun counsel-flycheck ()
    (interactive)
    (if (not (bound-and-true-p flycheck-mode))
        (message "Flycheck mode is not available or enabled")
      (ivy-read "Error: "
                (let ((source-buffer (current-buffer)))
                  (with-current-buffer (or (get-buffer flycheck-error-list-buffer)
                                           (progn
                                             (with-current-buffer
                                                 (get-buffer-create flycheck-error-list-buffer)
                                               (flycheck-error-list-mode)
                                               (current-buffer))))
                    (flycheck-error-list-set-source source-buffer)
                    (flycheck-error-list-reset-filter)
                    (revert-buffer t t t)
                    (split-string (buffer-string) "\n" t " *")))
                :action (lambda (s &rest _)
                          (-when-let* ( (error (get-text-property 0 'tabulated-list-id s))
                                        (pos (flycheck-error-pos error)) )
                            (goto-char (flycheck-error-pos error))))
                :history 'counsel-flycheck-history)))
  :bind
  (:map mode-specific-map
        ("y" . counsel-flycheck))
  :custom-face (flycheck-warning ((t (:foreground "yellow" :background "red"))))
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (flycheck-display-errors-delay 0.4)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (flycheck-global-modes '(not emacs-lisp-mode))
  :config
  (global-flycheck-mode 1))

(use-package format-all
  :after copy-as-format
  :bind
  (:map custom-formatting-map
        ("b" . format-all-buffer)))

(use-package multiple-cursors
  :after region-bindings-mode
  :bind
  (:map region-bindings-mode-map
        ("C-*" . mc/mark-all-like-this)
        ("C-S-<up>" . mc/mark-previous-like-this)
        ("C-S-<down>" . mc/mark-next-like-this)
        ("C-%" . mc/mark-more-like-this-extended)))
;; You must add swiper-mc to the mc/cmds-to-run-once list. I messed this up initially and had to fix it by hand.

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] . mwim-end-of-code-or-line))

(use-package paren
  :custom
  (show-paren-delay 0)
  (show-paren-mode t))

(use-package recursive-narrow
  :bind
  (:map mode-specific-map
        :prefix-map custom-narrowing-map
        :prefix "n"
        ("r" . narrow-to-region)
        ("d" . narrow-to-defun)
        ("w" . widen)
        ("N" . recursive-narrow-or-widen-dwim)
        ("D" . recursive-widen-dwim)))

(use-package region-bindings-mode
  :custom
  (region-bindings-mode-disable-predicates '((lambda () buffer-read-only)))
  :config
  (region-bindings-mode-enable))

(use-package select
  :custom
  (select-enable-clipboard t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package shift-number
  :bind
  ("M-+" . shift-number-up)
  ("M-_" . shift-number-down))

(use-package simple
  :delight
  ((visual-line-mode . " ↩")
   (auto-fill-function . " ↵"))
  :preface
  (defun custom/newline-hook ()
    (local-set-key (kbd "C-m") 'newline-and-indent)
    (local-set-key (kbd "<return>") 'newline-and-indent))
  (defun custom/gnocchi-case (s)
    "Convert S to 'gnocchi case'."
    (declare (side-effect-free t))
    (s-join " " (mapcar 'downcase (s-split-words s))))
  (defun custom/switch-case (&optional style)
    (interactive)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (from (if (use-region-p) (region-beginning) (car bounds)))
           (to (if (use-region-p) (region-end) (cdr bounds)))
           (data (buffer-substring-no-properties from to))
           (result (funcall (cond ((eq style 'camel) 's-lower-camel-case)
                                  ((eq style 'camel-up) 's-upper-camel-case)
                                  ((eq style 'snake) 's-snake-case)
                                  ((eq style 'gnocchi) 'custom/gnocchi-case)
                                  ((eq style 'kebab) 's-dashed-words)
                                  (t 's-lower-camel-case))
                            data)))
      (save-excursion
        (delete-region from to)
        (goto-char from)
        (insert result))))
  :hook (((yaml-mode-hook emacs-lisp-mode-hook lisp-mode-hook python-mode-hook) . custom/newline-hook)
         ((prog-mode-hook text-mode-hook) . turn-on-auto-fill)
         (eval-expression-minibuffer-setup-hook . eldoc-mode))
  :bind
  (("M-g" . goto-line)
   ("M-SPC" . cycle-spacing)
   :prefix-map misc-editing-map
   :prefix "<f11>"
   ("b" . subword-mode)
   ("v" . view-mode)
   :prefix-map common-editing-map
   :prefix "C-z"
   ("o" . cycle-spacing)
   ("w" . delete-trailing-whitespace)
   ("s" . transpose-sexps)
   ("6" . (lambda () (interactive) (custom/switch-case 'camel)))
   ("^" . (lambda () (interactive) (custom/switch-case 'camel-up)))
   ("_" . (lambda () (interactive) (custom/switch-case 'snake)))
   ("SPC" . (lambda () (interactive) (custom/switch-case 'gnocchi)))
   ("-" . (lambda () (interactive) (custom/switch-case 'kebab))))
  :custom
  (bidi-display-reordering nil)
  (kill-whole-line t)
  (next-line-add-newlines nil)
  (blink-matching-paren nil)
  (set-mark-command-repeat-pop nil)
  (save-interprogram-paste-before-kill t)
  (x-gtk-use-system-tooltips nil)
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)
  (kill-ring-max 1024)
  :config
  (column-number-mode 1)
  (line-number-mode 1)
  (size-indication-mode 1)
  (toggle-truncate-lines 1)
  (transient-mark-mode -1)
  (put 'transient-mark-mode 'permanent-local t)
  (put 'set-goal-column 'disabled nil))

(use-package smartparens
  :after dash
  :demand t
  :hook
  ((prog-mode-hook yaml-mode-hook) . smartparens-mode)
  ((lisp-mode-hook emacs-lisp-mode-hook markdown-mode-hook) . smartparens-strict-mode)
  (eval-expression-minibuffer-setup-hook . smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ;;TODO: try to make more brief keybindings
        ("M-<backspace>" . nil)
        ("M-B" . nil)
        ("M-F" . nil)
        ("C-M-t" . sp-transpose-sexp)
        ("C-S-a" . sp-beginning-of-sexp)
        ("C-S-d" . sp-end-of-sexp)
        ("C-<left_bracket>" . sp-select-previous-thing))
  :config
  (use-package smartparens-config)
  (show-smartparens-global-mode t)
  (sp-use-smartparens-bindings))

(use-package sort
  :bind
  (:map mode-specific-map
        :prefix-map custom-sorting-map
        :prefix "s"
        ("s" . sort-lines)
        ("u" . delete-duplicate-lines)))

(use-package tabify
  :bind
  (:map mode-specific-map
        :prefix-map custom-tabs-map
        :prefix "t"
        ("SPC" . untabify)
        ("TAB" . tabify)))

(use-package undo-propose
  :bind
  (:map ctl-x-map
        ("u" . undo-propose)))

(use-package wgrep
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)
        ("C-c C-c" . wgrep-finish-edit)))

(use-package whitespace
  :hook
  ((prog-mode-hook text-mode-hook) . whitespace-turn-on)
  :bind
  (:map mode-specific-map
        :prefix-map custom-ws-map
        :prefix "x"
        ("w" . whitespace-mode)
        ("W" . global-whitespace-mode))
  :custom
  (whitespace-line-column 121)
  (whitespace-style '(indentation::space
                      space-after-tab
                      space-before-tab
                      trailing
                      lines-tail
                      tab-mark
                      face
                      tabs)))

(use-package fancy-dabbrev
  :bind
  ("C-<tab>" . fancy-dabbrev-expand-or-indent)
  ("<backtab>" . fancy-dabbrev-backward)
  :config
  (global-fancy-dabbrev-mode)
  :custom
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  (fancy-dabbrev-menu-height 15)
  (fancy-dabbrev-sort-menu t)
  (fancy-dabbrev-preview-context 'before-non-word))

(use-package aggressive-indent
  :bind
  (:map mode-specific-map
        ("a" . aggressive-indent-mode))
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package ws-butler
  :hook (after-init-hook . ws-butler-global-mode)
  :custom
  (ws-butler-convert-leading-tabs-or-spaces t)
  (ws-butler-global-exempt-modes '(markdown-mode)))

(use-package yasnippet
  :demand t
  :delight yas-minor-mode
  :mode (("@emacsYasnippetSnippets@" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode))
  :preface
  (defun custom/update-yasnippets-on-save ()
    (when (string-match "@emacsYasnippetSnippets@" buffer-file-name)
      (yas-load-directory "@emacsResourcesDir@")))
  :bind
  ;; (:map custom-yasnippet-map
  ;;      ("v" . yas-visit-snippet-file))
  (:map yas-keymap
        ([(tab)] . nil)
        ([(shift tab)] . nil)
        ([backtab] . nil)
        ("TAB" . nil)
        ("<return>" . yas-exit-all-snippets))
  (:map yas-minor-mode-map
        ([(tab)] . nil)
        ("<tab>" . nil)
        ("TAB" . nil))
  :hook
  (hippie-expand-try-functions-list . yas-hippie-try-expand)
  (after-init-hook . yas-global-mode)
  (after-save-hook . custom/update-yasnippets-on-save)
  :custom
  (yas-key-syntaxes '("w" "w_" "w_." "^ " "w_.()" yas-try-key-from-whitespace))
  (yas-expand-only-for-last-commands '(self-insert-command))
  (yas-prompt-functions '(yas-completing-prompt
                          yas-x-prompt
                          yas-no-prompt))
  (yas-wrap-around-region t)
  (yas-snippet-dirs '(yas-installed-snippets-dir
                      "@emacsYasnippetSnippets@")))
