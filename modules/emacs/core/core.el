(setq load-prefer-newer t)
(setq message-log-max t) ;; we don't want to lose any startup log info

(when (eq system-type 'gnu/linux)
  (let ((linux-release (shell-command-to-string "lsb_release -sd")))
    (setq shell-file-name
          (cond ((string-match-p "NixOS" linux-release) "/run/current-system/sw/bin/bash")
                (t "/bin/bash")))))

(setq gc-cons-percentage 0.5)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 800000)))

(setq initial-major-mode 'fundamental-mode)
(setq frame-inhibit-implied-resize t)

(require 'cl)
(require 'package)
(require 'subr-x) ;; NOTE: for those packages where it was suddenly forgotten

(when @fallbackPackageArchives@
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/")))

(unless package--initialized
  (package-initialize))
(setq package-enable-at-startup nil)

(mapcar
 (lambda (package)
   (unless (package-installed-p package)
     (unless package-archive-contents
       (package-refresh-contents))
     (package-install package)))
 '(use-package))

(setq use-package-compute-statistics t)
(setq use-package-verbose t)
(setq use-package-hook-name-suffix "")
(put 'use-package 'lisp-indent-function 1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(global-set-key (kbd "C-x C-.") #'goto-char)

(require 'anaphora) ;TODO: use-package

(use-package auto-compile
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

(use-package compdef)

(require 'deferred) ;TODO: use-package

(require 'delight) ;TODO: use-package

(require 'f) ;TODO: use-package

(use-package no-littering
  :custom
  (no-littering-var-directory "@emacsDatadir@/"))

(use-package emacs
  :preface
  (defun custom/kill-buffer ()
    (interactive)
    (kill-buffer nil))
  :bind
  ("M-\"" . eval-region)
  (:map ctl-x-map
        ("k" . custom/kill-buffer))
  :hook
  (minibuffer-setup-hook . (lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (minibuffer-exit-hook . (lambda () (setq gc-cons-threshold 800000)))
  (focus-out-hook . garbage-collect)
  (minibuffer-setup-hook . cursor-intangible-mode)
  :custom
  (use-dialog-box nil)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t point-entered cursor-intangible t minibuffer-avoid-prompt face minibuffer-prompt))
  (undo-limit 1000000)
  (indent-tabs-mode nil)
  (inhibit-eol-conversion t)
  (mark-even-if-inactive nil)
  (x-stretch-cursor t)
  (print-circle t)
  (print-gensym t)
  (sentence-end-double-space nil)
  (tab-always-indent 'complete)
  (split-width-threshold nil)
  (split-height-threshold nil)
  (scroll-preserve-screen-position 'always)
  (cursor-type 'bar)
  (completion-cycle-threshold 3)
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-charset-priority 'unicode)
  (set-default 'indent-tabs-mode nil) ;; Never insert tabs, !!!DO NOT REMOVE!!!
  (when (version<= "28.0.50" emacs-version)
    (setq read-extended-command-predicate #'command-completion-default-include-p))
  (setq-default tab-width 4)
  (setq-default fill-column 200)
  (setq-default indicate-empty-lines t)
  (setq-default truncate-lines t))

(use-package select
  :custom
  (select-enable-clipboard t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
