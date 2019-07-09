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
  (use-package company-flx
    :ensure t
    :config
    (company-flx-mode 1))
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
  :hook (python-mode-hook . ws-butler-mode)
  :custom
  (ws-butler-convert-leading-tabs-or-spaces t)
  (ws-butler-global-exempt-modes '(markdown-mode)))

(use-package yasnippet
  :ensure t
  :demand t
  :delight yas-minor-mode
  :mode (("@emacsCustomYasnippetsDir@" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode))
  :preface
  (defun custom/update-yasnippets-on-save ()
    (when (string-match "@emacsCustomYasnippetsDir@" buffer-file-name)
      (yas-load-directory "@emacsResourcesDir@/")))
  :bind
  (:map custom-yasnippet-map
        ("v" . yas-visit-snippet-file))
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
                      "@emacsCustomYasnippetsDir@")))
