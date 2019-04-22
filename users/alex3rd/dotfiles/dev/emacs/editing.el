(use-package reverse-im
  :ensure t
  :if (not (member (getenv "CURRENT_WM") '("stumpwm" "xmonad")))
  :config
  (reverse-im-activate "russian-computer"))

(use-package mule
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-buffer-file-coding-system 'utf-8 'utf-8-unix)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(setq default-input-method 'russian-computer)

(use-package wgrep
  :ensure t
  :general
  (:keymaps 'grep-mode-map
            "C-x C-q" 'wgrep-change-to-wgrep-mode
            "C-c C-c" 'wgrep-finish-edit))

(use-package ialign
  :ensure t
  :general
  (:keymaps 'ctl-x-map
            "l" 'ialign))

(use-package multiple-cursors
  :ensure t
  :general
  (:keymaps 'region-bindings-mode-map
            "C-*" 'mc/mark-all-like-this
            "C-S-<up>" 'mc/mark-previous-like-this
            "C-S-<down>" 'mc/mark-next-like-this
            "C-%" 'mc/mark-more-like-this-extended))

(use-package delsel
  :general
  (:keymaps 'mode-specific-map
            "C-g" 'minibuffer-keyboard-quit)
  :config
  (delete-selection-mode 1))

;;TODO: bind to keys more extensively
;;TODO: consolidate (un)filling functionality
(use-package unfill
  :ensure t
  :general
  ([remap fill-paragraph] 'unfill-toggle))

(use-package simple
  :hook
  (((prog-mode-hook text-mode-hook) . turn-on-auto-fill))
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
  :hook (((yaml-mode-hook emacs-lisp-mode-hook lisp-mode-hook python-mode-hook) . custom/newline-hook))
  :general
  ("M-g" 'goto-line)
  ("M-SPC" 'cycle-spacing)
  (:prefix "<f11>"
           "b" 'subword-mode
           "v" 'view-mode)
  (:prefix "C-z"
           "o" 'just-one-space
           "w" 'delete-trailing-whitespace
           "s" 'transpose-sexps
           "6" '(lambda () (interactive) (custom/switch-case 'camel))
           "^" '(lambda () (interactive) (custom/switch-case 'camel-up))
           "_" '(lambda () (interactive) (custom/switch-case 'snake))
           "SPC" '(lambda () (interactive) (custom/switch-case 'gnocchi))
           "-" '(lambda () (interactive) (custom/switch-case 'kebab)))
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
  (general-unbind 'global "<f11>" "C-z")
  (column-number-mode 1)
  (line-number-mode 1)
  (size-indication-mode 1)
  (toggle-truncate-lines 1)
  (transient-mark-mode 1)
  (put 'transient-mark-mode 'permanent-local t)
  (put 'set-goal-column 'disabled nil))

(use-package tabify
  :general
  (:keymaps 'mode-specific-map
            "t SPC" 'untabify
            "t TAB" 'tabify))

(use-package sort
  :general
  (:keymaps 'mode-specific-map
            "s s" 'sort-lines
            "s u" 'delete-duplicate-lines))

(use-package easy-kill
  :ensure t
  :general
  ([remap kill-ring-save] 'easy-kill)
  ([remap mark-sexp] 'easy-mark))

(use-package region-bindings-mode
  :ensure t
  :custom
  (region-bindings-mode-disable-predicates '((lambda () buffer-read-only)))
  :config
  (region-bindings-mode-enable))

(use-package recursive-narrow
  :ensure t
  :general
  (:keymaps 'mode-specific-map
            "n r" 'narrow-to-region
            "n d" 'narrow-to-defun
            "n w" 'widen
            "n N" 'recursive-narrow-or-widen-dwim
            "n D" 'recursive-widen-dwim))

(use-package highlight-indent-guides
  :ensure t
  :general
  (:keymaps 'mode-specific-map
            "g h" 'highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'fill)
  (highlight-indent-guides-responsive 'stack))

(use-package comment-dwim-2
  :ensure t
  :general
  (:keymaps 'mode-specific-map
            "]" 'comment-dwim-2))

(use-package newcomment
  :general
  (:keymaps 'mode-specific-map
            "/" 'comment-box))

(use-package select
  :custom
  (select-enable-clipboard t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package undo-propose
  :ensure t
  :general
  (:keymaps 'ctl-x-map
            "u" 'undo-propose))

(use-package electric
  :config
  (electric-indent-mode -1))

(use-package editorconfig
  :ensure t
  :delight (editorconfig-mode " EC")
  :hook ((prog-mode-hook text-mode-hook) . editorconfig-mode))

(use-package copy-as-format
  :ensure t
  :general
  (:keymaps 'mode-specific-map
            "f s" 'copy-as-format-slack
            "f g" 'copy-as-format-github
            "f o" 'copy-as-format-org-mode
            "f m" 'copy-as-format-markdown
            "f a" 'copy-as-format-asciidoc
            "f b" 'copy-as-format-bitbucket
            "f d" 'copy-as-format-disqus
            "f l" 'copy-as-format-gitlab
            "f c" 'copy-as-format-hipchat
            "f h" 'copy-as-format-html
            "f j" 'copy-as-format-jira
            "f w" 'copy-as-format-mediawiki
            "f p" 'copy-as-format-pod
            "f r" 'copy-as-format-rst
            "f f" 'copy-as-format))

(use-package whitespace
  :ensure t
  :defer 2
  :hook
  ((prog-mode-hook text-mode-hook) . whitespace-turn-on)
  :general
  (:keymaps 'mode-specific-map
            "x w" 'whitespace-mode
            "x W" 'global-whitespace-mode)
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

;;TODO: consolidate all whitespaces utils
;;TODO: think of activating ws-butler in some modes, just for hands-on testing
(use-package ws-butler
  :ensure t
  :commands ws-butler-mode)

(use-package yasnippet ;;TODO: make more declarative
  :ensure t
  :demand t
  :delight yas-minor-mode
  :mode (("yasnippet/snippets" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode))
  :preface
  ;; hook for automatic reloading of changed snippets
  (defun custom/update-yasnippets-on-save ()
    (when (string-match "/resources/yasnippet" buffer-file-name)
      (yas-load-directory (at-config-basedir "resources/"))))
  ;; Inter-field navigation
  (defun custom/yas-goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line)
        (goto-char position))))
  (defun custom/yas-goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line)
        (goto-char position))))
  (defun custom/do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))
  (defun custom/tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (custom/do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))
  :general
  (:prefix "<f5>"
           "v" 'yas-visit-snippet-file
           "i" 'ivy-yasnippet)
  (:keymaps 'yas-keymap
            [(tab)] nil
            [(shift tab)] nil
            [backtab] nil
            "TAB" nil
            "<return>" 'yas-exit-all-snippets
            "C-e" 'custom/yas-goto-end-of-active-field
            "C-a" 'custom/yas-goto-start-of-active-field
            "C-n" 'yas-next-field-or-maybe-expand
            "C-p" 'yas-prev-field)
  (:keymaps 'yas-minor-mode-map
            [(tab)] nil
            "<tab>" nil
            "TAB" nil)
  :hook
  (hippie-expand-try-functions-list . yas-hippie-try-expand)
  (after-save-hook . custom/update-yasnippets-on-save)
  :config
  ;; snippets editing mode
  (use-package ivy-yasnippet
    :after (dash ivy yasnippet)
    :ensure t)
  (setq yas-snippet-dirs nil)
  (push yas-installed-snippets-dir yas-snippet-dirs)
  (push (at-config-basedir "resources/yasnippet/") yas-snippet-dirs)
  (setq yas-key-syntaxes '("w" "w_" "w_." "^ " "w_.()" yas-try-key-from-whitespace))
  (setq yas-expand-only-for-last-commands '(self-insert-command))
  (setq yas-prompt-functions
        '(yas-completing-prompt
          yas-x-prompt
          yas-no-prompt))
  ;; Wrap around region
  (setq yas-wrap-around-region t)
  (yas-global-mode 1))

(use-package company
  :ensure t
  :demand t
  :delight (company-mode " γ")
  :general
  (:keymaps 'company-active-map
            "C-n" 'company-select-next
            "C-p" 'company-select-previous
            "C-d" 'company-show-doc-buffer
            "M-." 'company-show-location)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  :config
  (use-package company-flx
    :ensure t
    :no-require t
    :after (company)
    :config
    (company-flx-mode +1))
  (use-package company-quickhelp
    :ensure t
    :no-require t
    :after (company)
    :general
    (:keymaps 'company-active-map
              "C-c h" 'company-quickhelp-manual-begin)
    :config
    (company-quickhelp-mode 1))
  (use-package company-statistics
    :ensure t
    :after (company)
    :config
    (company-statistics-mode))
  (global-company-mode))

(use-package hippie-exp
  :general
  ("C-S-<iso-lefttab>" 'hippie-expand)
  :custom
  (setq hippie-expand-try-functions-list
        '(yas-hippie-try-expand
          try-expand-all-abbrevs
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-from-kill
          try-expand-dabbrev-all-buffers
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package abbrev
  :delight (abbrev-mode " Abv")
  :config
  (setq-default abbrev-mode t))

(use-package smartparens
  :ensure t
  :after (dash)
  :demand t
  :hook (((prog-mode-hook yaml-mode-hook) . smartparens-mode)
         ((lisp-mode-hook emacs-lisp-mode-hook markdown-mode-hook) . smartparens-strict-mode))
  :general
  (:keymaps 'smartparens-mode-map
         ;;TODO: try to make more brief keybindings
            "C-M-t" 'sp-transpose-sexp
            "M-F" nil
            "M-B" nil
            "M-<backspace>" nil
            "C-S-a" 'sp-beginning-of-sexp
            "C-S-d" 'sp-end-of-sexp
            ")" 'sp-up-sex
            "C-<left_bracket>" 'sp-select-previous-thing)
  :config
  (use-package smartparens-config)
  (show-smartparens-global-mode t)
  (sp-use-smartparens-bindings))

(use-package paren
  :defer 2
  :custom
  (show-paren-delay 0)
  :config
  (show-paren-mode t))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-mode))

(use-package aggressive-indent
  :ensure t
  :if enable-experimental-packages
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'slime-repl-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'lisp-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'emacs-lisp-mode)
  (delete 'lisp-mode aggressive-indent-modes-to-prefer-defun)
  (delete 'emacs-lisp-mode aggressive-indent-modes-to-prefer-defun)
  (add-to-list 'aggressive-indent-dont-indent-if
               '(not (null (string-match (rx (zero-or-more space) (syntax comment-start) (zero-or-more anything)) (thing-at-point 'line))))))

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
  :general
  (:keymaps 'mode-specific-map
            "y" 'counsel-flycheck)
  :custom-face (flycheck-warning ((t (:foreground "yellow" :background "red"))))
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (flycheck-display-errors-delay 0.4)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (flycheck-global-modes '(not emacs-lisp-mode))
  :config
  (global-flycheck-mode 1))

(use-package flycheck-pos-tip
  :ensure t
  :after (flycheck)
  :config
  (flycheck-pos-tip-mode 1))

(use-package zop-to-char
  :ensure t
  :general
  ([remap zap-to-char] 'zop-to-char))
