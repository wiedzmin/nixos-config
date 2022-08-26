(use-package executable
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package jka-cmpr-hook
  :config
  (auto-compression-mode 1))

(use-package novice
  :custom
  (disabled-command-function nil))

(use-package hl-todo
  :hook
  (prog-mode-hook . hl-todo-mode))

(use-package copy-as-format
  :bind
  (:map custom-formatting-map
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

(use-package flycheck
  :delight
  :custom-face (flycheck-warning ((t (:foreground "yellow" :background "red"))))
  :custom
  (flycheck-global-modes '(not emacs-lisp-mode org-mode))
  (flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (flycheck-display-errors-delay 0.4)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package format-all
  :after copy-as-format
  :bind
  (:map custom-formatting-map
        ("b" . format-all-buffer)))

(use-package simple
  :delight auto-fill-function
  :hook (((prog-mode-hook text-mode-hook) . turn-on-auto-fill)
         (eval-expression-minibuffer-setup-hook . eldoc-mode))
  :bind
  ("M-SPC" . cycle-spacing)
  (:map misc-editing-map
        ("b" . subword-mode)
        ("v" . view-mode)
        ("t" . transpose-sexps))
  (:map token-editing-map
        ("o" . cycle-spacing)
        ("w" . delete-trailing-whitespace))
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
  (delight 'eldoc-mode nil "eldoc")
  (put 'transient-mark-mode 'permanent-local t)
  (put 'set-goal-column 'disabled nil))

(use-package eldoc
  :config
  (global-eldoc-mode -1))
