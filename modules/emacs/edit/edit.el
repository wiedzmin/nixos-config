(use-package expand-region
  :bind
  ("C-," . er/expand-region))

(use-package highlight-numbers
  :hook
  (foo-mode-hook . highlight-numbers-mode))

(use-package aggressive-indent
  :bind
  (:map mode-specific-map
        ("a" . aggressive-indent-mode))
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package evil-nerd-commenter
  :bind
  ;TODO: bind #'evilnc-toggle-invert-comment-line-by-line
  (:map mode-specific-map
        ("]" . evilnc-comment-or-uncomment-lines)
        ("}" . evilnc-copy-and-comment-lines)
        ("\\" . evilnc-comment-and-kill-ring-save)
        ("|" . evilnc-comment-or-uncomment-paragraphs)))

(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

(use-package multiple-cursors
  :after region-bindings-mode
  :bind
  (:map region-bindings-mode-map
        ("C-*" . mc/mark-all-like-this)
        ("C-S-<up>" . mc/mark-previous-like-this)
        ("C-S-<down>" . mc/mark-next-like-this)
        ("C-%" . mc/mark-more-like-this-extended)))

(use-package shift-number
  :bind
  ("M-+" . shift-number-up)
  ("M-_" . shift-number-down))

(use-package smartparens
  :after dash
  :hook
  ((prog-mode-hook yaml-mode-hook) . smartparens-mode)
  ((prog-mode-hook yaml-mode-hook) . show-smartparens-mode)
  ((lisp-mode-hook emacs-lisp-mode-hook markdown-mode-hook) . smartparens-strict-mode)
  (eval-expression-minibuffer-setup-hook . smartparens-mode)
  :bind
  ("M-e" . sp-splice-sexp)
  (:map smartparens-mode-map
        ("C-M-t" . sp-transpose-sexp)
        ("M-s" . nil)
        ("M-e" . sp-splice-sexp))
  (:map sp-keymap
        ("C-M-t" . sp-transpose-sexp)
        ("M-s" . nil)
        ("M-e" . sp-splice-sexp))
  :config
  (use-package smartparens-config)
  (sp-use-paredit-bindings)
  :custom
  (sp-show-pair-delay 0.0)
  :custom-face
  (sp-show-pair-match-face
   ((t (:inherit ace-jump-face-foreground
                 :foreground "green" :background "#000000"))))
  (sp-show-pair-mismatch-face
   ((t (:inherit ace-jump-face-foreground
                 :foreground "red" :background "#000000"))))
  (sp-show-pair-match-content-face
   ((t (:inherit ace-jump-face-foreground
                 :foreground "#fcba03")))))

(use-package string-inflection
  :preface
  (defun custom/string-inflection-gnocchi ()
    "foo_bar => foo bar"
    (interactive)
    (let ((case-fold-search nil)
          (str (string-inflection-get-current-word)))
      (setq str (string-inflection-underscore-function str))
      (setq str (replace-regexp-in-string "_" " " str))
      (string-inflection-insert str)))
  :commands (string-inflection-get-current-word)
  :bind
  (:map common-editing-map
        ("6" . string-inflection-lower-camelcase)
        ("^" . string-inflection-camelcase)
        ("_" . string-inflection-underscore)
        ("-" . string-inflection-kebab-case)
        ("SPC" . custom/string-inflection-gnocchi)))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)
        ("C-c C-c" . wgrep-finish-edit))
  :custom
  (wgrep-auto-save-buffer t))

(use-package ws-butler
  :after whitespace
  :bind
  (:map custom-ws-map
        ("b" . ws-butler-mode)
        ("B" . ws-butler-global-mode))
  :custom
  (ws-butler-convert-leading-tabs-or-spaces t)
  (ws-butler-global-exempt-modes '(markdown-mode go-mode))
  (ws-butler-global-mode))

(use-package region-bindings-mode
  :delight
  :custom
  (region-bindings-mode-disable-predicates '((lambda () buffer-read-only)))
  :config
  (region-bindings-mode-enable))

(use-package whole-line-or-region
  :bind
  (:map mode-specific-map
        ("w" . whole-line-or-region-local-mode)))

(use-package sort
  :bind
  (:map custom-sorting-map
        ("s" . sort-lines)
        ("u" . delete-duplicate-lines)))

(use-package tabify
  :bind
  (:map custom-tabs-map
        ("SPC" . untabify)
        ("TAB" . tabify)))

(use-package view
  :delight view-mode)

(use-package delsel
  :config
  (delete-selection-mode 1))

(use-package undo-propose
  :bind
  (:map ctl-x-map
        ("u" . undo-propose)))

(use-package kmacro
  :custom
  (setq kmacro-ring-max 16))

(use-package autorevert
  :if (string-equal "i3" (getenv "CURRENT_WM"))
  :custom
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  :config
  (delight 'auto-revert-mode " ⟲" 'autorevert)
  (global-auto-revert-mode 1))

(use-package misc
  :bind
  (:map mode-specific-map
        ("^" . copy-from-above-command)))

(use-package simple
  :bind
  (:map misc-editing-map
        ("c" . clone-buffer)
        ("C" . clone-indirect-buffer)
        ("i" . clone-indirect-buffer-other-window)))

(use-package drag-stuff
  :bind
  (:map mode-specific-map
        ("<up>" . drag-stuff-up)
        ("<down>" . drag-stuff-down)
        ("<left>" . drag-stuff-left)
        ("<right>" . drag-stuff-right)))
