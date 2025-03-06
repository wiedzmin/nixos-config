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

(use-package easy-kill-extras
  :after easy-kill
  :load-path "@emacsEasyKillExtrasPath@")

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
  (:map token-editing-map
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

(use-package whitespace
  :hook
  ((prog-mode-hook text-mode-hook) . whitespace-turn-on)
  (org-mode-hook . whitespace-turn-off)
  :bind
  (:map custom-ws-map
        ("w" . whitespace-mode))
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
  (:map misc-editing-map
        ("s" . sort-lines)
        ("u" . delete-duplicate-lines)))

(use-package tabify
  :bind
  (:map misc-editing-map
        ("SPC" . untabify)
        ("TAB" . tabify)))

(use-package view
  :delight view-mode)

(use-package delsel
  :config
  (delete-selection-mode 1))

;;TODO: opt for `vundo' in case of any errors
(use-package undo-tree
  :bind
  (:map ctl-x-map
        ("u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1))

(use-package kmacro
  :custom
  (setq kmacro-ring-max 16))

(use-package misc
  :bind
  (:map mode-specific-map
        ("^" . copy-from-above-command)
        ("2" . duplicate-dwim)
        ("@" . duplicate-line)))

(use-package simple
  :bind
  (:map misc-editing-map
        ("c" . clone-buffer)
        ("C" . clone-indirect-buffer)
        ("I" . clone-indirect-buffer-other-window)))

(use-package drag-stuff
  :bind
  (:map mode-specific-map
        ("<up>" . drag-stuff-up)
        ("<down>" . drag-stuff-down)
        ("<left>" . drag-stuff-left)
        ("<right>" . drag-stuff-right)))

(use-package hippie-expand
  :bind
  ([remap dabbrev-expand] . hippie-expand))

(use-package misearch
  :bind
  (:map misc-editing-map
        ("r" . replace-regexp-as-diff)
        ("m" . multi-file-replace-regexp-as-diff)
        ("d" . dired-do-replace-regexp-as-diff)))

(use-package focus
  :bind
  ;; FIXME: elaborate more modally local keybindgs
  ;; NOTE: currently they are binded as is for the sake of keeping
  (:map mode-specific-map
        ("." . focus-mode)
        ("," . focus-read-only-mode)
        (">" . focus-pin)
        ("<" . focus-unpin)
        ("6" . focus-change-thing)
        ("0" . focus-next-thing)
        ("9" . focus-prev-thing))
  :config
  ;; NOTE: global question, not to forget - where should such inter-package settings reside? on which side?
  (add-to-list 'focus-mode-to-thing '(python-mode . paragraph)))
