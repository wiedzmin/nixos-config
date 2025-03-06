(setq load-prefer-newer t)
(setq message-log-max t) ;; we don't want to lose any startup log info

(when (eq system-type 'gnu/linux)
  (let ((linux-release (shell-command-to-string "lsb_release -sd")))
    (setq shell-file-name
          (cond ((string-match-p "NixOS" linux-release) "/run/current-system/sw/bin/bash")
                (t "/bin/bash")))))

(let ((old-threshold gc-cons-threshold))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold old-threshold)))
  (setq gc-cons-threshold most-positive-fixnum))

(setq initial-major-mode 'fundamental-mode)
(setq frame-inhibit-implied-resize t)

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

(use-package anaphora)
(use-package deferred)
(use-package delight)
(use-package f)

(use-package no-littering
  :custom
  (no-littering-var-directory "@emacsDatadir@/"))

(use-package emacs
  :preface
  (defun custom/kill-buffer ()
    (interactive)
    (kill-buffer nil))
  :mode (("\\.js$" . js-json-mode)
         ("\\.json$" . js-json-mode))
  :bind
  ("M-\"" . eval-region)
  (:map ctl-x-map
        ("k" . custom/kill-buffer)
        ("K" . kill-matching-buffers-no-ask))
  (:map mode-specific-map
        ("C-r" . rename-visited-file))
  (:map custom-goto-map
        ("TAB" . move-to-column)
        ("c" . goto-char))
  :hook
  (minibuffer-setup-hook . (lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (minibuffer-exit-hook . (lambda () (setq gc-cons-threshold 800000)))
  (focus-out-hook . garbage-collect)
  (minibuffer-setup-hook . cursor-intangible-mode)
  :custom
  (use-dialog-box nil "Disable dialog boxes")
  (create-lockfiles nil)
  (enable-recursive-minibuffers t "allow minibuffer commands in the minibuffer")
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
  (completion-ignored-extensions nil)
  (window-combination-resize t)
  (y-or-n-p-use-read-key t)
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

(use-package cus-edit
  :bind
  (:map mode-specific-map
        ("e" . customize-dirlocals))
  :custom
  (custom-file (locate-user-emacs-file "custom-vars.el"))
  :config
  (load custom-file 'noerror 'nomessage))

(use-package select
  :custom
  (select-enable-clipboard t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package password-menu
  :load-path "@emacsPasswordMenuPath@"
  :bind
  (:map custom-goto-map
        ("j" . password-menu-transient)
        ("J" . password-menu-completing-read)))
