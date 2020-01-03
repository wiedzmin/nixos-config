let
  deps = import ../../nix/sources.nix;
in
{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.ide.emacs;
  baseSetup = ''
    (setq load-prefer-newer t)
    (setq message-log-max t) ;; we don't want to lose any startup log info
    (setq shell-file-name "${pkgs.bash}/bin/bash")

    (setq gc-cons-percentage 0.3)

    (setq gc-cons-threshold most-positive-fixnum)

    (add-hook 'after-init-hook #'(lambda ()
                                   (setq gc-cons-threshold 800000)))

    (add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
    (add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))

    (add-hook 'focus-out-hook #'garbage-collect)

    (when (and (>= libgnutls-version 30603)
                (version<= emacs-version "26.2"))
      (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

    (require 'cl)
    (require 'package)

    (unless package--initialized
      (package-initialize))
    (setq package-enable-at-startup nil)

    (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

    (setf (cdr (assoc "gnu" package-archives))
          "https://elpa.gnu.org/packages/")
    (setq package-archive-priorities
          '(("melpa-stable" . 5)
            ("gnu" . 5)
            ("melpa" . 10)))

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

    (use-package quelpa :ensure t :defer t)
    (use-package quelpa-use-package
      :ensure t
      :custom
      (quelpa-use-package-inhibit-loading-quelpa
       t "Improve startup performance"))
    (use-package anaphora :ensure t)

    (pinentry-start)

    (use-package deferred :ensure t)

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
      :ensure t
      :bind
      ("M-x" . amx)
      :custom
      (amx-backend 'ivy)
      (amx-save-file "${cfg.dataDir}/amx-items"))

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
      (custom-file "/home/${config.attributes.mainUser.name}/.emacs.d/customizations.el"))

    (use-package delight :ensure t)

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
      (no-littering-var-directory "${cfg.dataDir}/"))

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

    (imagemagick-register-types)

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

    (use-package beginend
      :ensure t
      :delight beginend-global-mode beginend-prog-mode beginend-magit-status-mode
      :config
      (beginend-global-mode))

    (use-package comment-dwim-2
      :ensure t
      :bind
      (:map mode-specific-map
            ("]" . comment-dwim-2))
      :custom
      (cd2/region-command 'cd2/comment-or-uncomment-region))

    (use-package company
      :ensure t
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
        :ensure t
        :custom
        (company-fuzzy-prefix-ontop t)
        (company-fuzzy-show-annotation t)
        (company-fuzzy-sorting-backend 'alphabetic)
        :config
        (add-to-list 'company-fuzzy--no-prefix-backends 'company-yasnippet)
        (global-company-fuzzy-mode 1))
      (use-package company-quickhelp
        :ensure t
        :config
        (company-quickhelp-mode 1))
      (use-package company-statistics
        :ensure t
        :config
        (company-statistics-mode)))

    (use-package copy-as-format
      :ensure t
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

    (use-package easy-kill
      :ensure t
      :bind
      ([remap kill-ring-save] . easy-kill)
      ([remap mark-sexp] . easy-mark))

    (use-package editorconfig
      :ensure t
      :delight (editorconfig-mode " EC")
      :hook ((prog-mode-hook text-mode-hook) . editorconfig-mode))

    (use-package flycheck
      :ensure t
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
      :ensure t
      :after copy-as-format
      :bind
      (:map custom-formatting-map
            ("b" . format-all-buffer)))

    (use-package multiple-cursors
      :ensure t
      :after region-bindings-mode
      :bind
      (:map region-bindings-mode-map
            ("C-*" . mc/mark-all-like-this)
            ("C-S-<up>" . mc/mark-previous-like-this)
            ("C-S-<down>" . mc/mark-next-like-this)
            ("C-%" . mc/mark-more-like-this-extended)))
    ;; You must add swiper-mc to the mc/cmds-to-run-once list. I messed this up initially and had to fix it by hand.

    (use-package mwim
      :ensure t
      :bind
      ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
      ([remap move-end-of-line] . mwim-end-of-code-or-line))

    (use-package paren
      :custom
      (show-paren-delay 0)
      (show-paren-mode t))

    (use-package recursive-narrow
      :ensure t
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
      :ensure t
      :custom
      (region-bindings-mode-disable-predicates '((lambda () buffer-read-only)))
      :config
      (region-bindings-mode-enable))

    (use-package select
      :custom
      (select-enable-clipboard t)
      (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

    (use-package shift-number
      :ensure t
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
             ((prog-mode-hook text-mode-hook) . turn-on-auto-fill))
      :bind
      (("M-g" . goto-line)
       ("M-SPC" . cycle-spacing)
       :prefix-map misc-editing-map
       :prefix "<f11>"
       ("b" . subword-mode)
       ("v" . view-mode)
       :prefix-map common-editing-map
       :prefix "C-z"
       ("o" . just-one-space)
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
      :ensure t
      :after dash
      :demand t
      :hook (((prog-mode-hook yaml-mode-hook) . smartparens-mode)
             ((lisp-mode-hook emacs-lisp-mode-hook markdown-mode-hook) . smartparens-strict-mode))
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

    ;; TODO: use-package'ize
    (dolist (mode '(paredit-mode smartparens-mode))
      (when (fboundp mode)
        (add-hook 'eval-expression-minibuffer-setup-hook mode)))

    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

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
      :ensure t
      :bind
      (:map ctl-x-map
            ("u" . undo-propose)))

    (use-package wgrep
      :ensure t
      :bind
      (:map grep-mode-map
            ("C-x C-q" . wgrep-change-to-wgrep-mode)
            ("C-c C-c" . wgrep-finish-edit)))

    (use-package whitespace
      :ensure t
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

    (use-package ws-butler
      :ensure t
      :hook (after-init-hook . ws-butler-global-mode)
      :custom
      (ws-butler-convert-leading-tabs-or-spaces t)
      (ws-butler-global-exempt-modes '(markdown-mode)))

    (use-package yasnippet
      :ensure t
      :demand t
      :delight yas-minor-mode
      :mode (("${deps.yasnippet-snippets}" . snippet-mode)
             ("\\.yasnippet$" . snippet-mode))
      :preface
      (defun custom/update-yasnippets-on-save ()
        (when (string-match "${deps.yasnippet-snippets}" buffer-file-name)
          (yas-load-directory "/home/${config.attributes.mainUser.name}/.emacs.d/resources/")))
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
                          "${deps.yasnippet-snippets}")))
  '';
in {
  options = {
    ide.emacs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs setup.";
      };
      config = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Content to be placed to init.el file.
        '';
      };
      dataDir = mkOption {
        type = types.str;
        default = "/home/${config.attributes.mainUser.name}/.emacs.d/data";
        visible = false;
        readOnly = true;
        internal = true;
        description = ''
          Path to store user data under.
        '';
      };
      orgDir = mkOption {
        type = types.str;
        default = "/home/${config.attributes.mainUser.name}/docs/org";
        description = ''
          Path to store org-mode docs under.
        '';
      };
      initElContent = mkOption {
        type = types.lines;
        default = ''
          ;; -*- lexical-binding: t -*-
          (setq debug-on-error t)
          (setq debug-on-quit t)

          ${baseSetup}
          ${cfg.config}
          (setq debug-on-error nil)
          (setq debug-on-quit nil)
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = ''
          Aggregated content of init.el file.
        '';
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      fonts = {
        fonts = with pkgs; [
          emacs-all-the-icons-fonts
        ];
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          (makeDesktopItem {
            name = "org-protocol";
            exec = "${emacs}/bin/emacsclient %U";
            comment = "";
            desktopName = "Custom org-protocol handler";
            categories = "System";
            mimeType = stdenv.lib.concatStringsSep ";" [ "x-scheme-handler/org-protocol" ];
          })

          ispell
        ];
        programs.zsh.sessionVariables = {
          EDITOR = "${pkgs.emacs}/bin/emacsclient";
        };
        programs.bash.sessionVariables = {
          EDITOR = "${pkgs.emacs}/bin/emacsclient";
        };
        programs.emacs = {
          enable = true;
          package = (pkgs.emacs26.override {
            # build Lucid version
            withGTK2 = false;
            withGTK3 = false;
          });
          # TODO: scan *.el and find packages not in list below
          extraPackages = epkgs: [
            epkgs.amx
            epkgs.anaphora
            epkgs.auto-compile
            epkgs.backup-each-save
            epkgs.beginend
            epkgs.blockdiag-mode
            epkgs.browse-at-remote
            epkgs.comment-dwim-2
            epkgs.company
            epkgs.company-fuzzy
            epkgs.company-lsp
            epkgs.company-quickhelp
            epkgs.company-statistics
            epkgs.copy-as-format
            epkgs.default-text-scale
            epkgs.deferred
            epkgs.delight
            epkgs.easy-kill
            epkgs.easy-kill-extras # add to .el
            epkgs.editorconfig
            epkgs.f
            epkgs.flycheck
            epkgs.format-all
            epkgs.gcmh
            epkgs.haskell-mode
            epkgs.hl-todo
            epkgs.ini-mode
            epkgs.iqa
            epkgs.keychain-environment
            epkgs.lsp-mode
            epkgs.lsp-ui
            epkgs.markdown-mode
            epkgs.multiple-cursors
            epkgs.mwim
            epkgs.no-littering
            epkgs.pinentry
            epkgs.posframe
            epkgs.quelpa
            epkgs.quelpa-use-package
            epkgs.recentf-ext
            epkgs.recursive-narrow
            epkgs.region-bindings-mode
            epkgs.restart-emacs
            epkgs.savekill
            epkgs.shift-number
            epkgs.smartparens
            epkgs.super-save
            epkgs.undo-propose
            epkgs.wgrep
            epkgs.ws-butler
            epkgs.yasnippet
          ];
        };
        home.file = {
          ".emacs.d/init.el".text = cfg.initElContent;
        };
      };
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-w S-e" = ''spawn "${pkgs.procps}/bin/pkill -SIGUSR2 emacs"'';
      };
    })
  ];
}
