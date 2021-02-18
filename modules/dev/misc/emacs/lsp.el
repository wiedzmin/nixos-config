(use-package lsp-mode
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration)
  :bind
  (:map mode-specific-map
        ("R" . lsp-workspace-restart))
  (:map lsp-mode-map
        ("C-M-r" . lsp-rename)
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  (:map mode-specific-map
        ("R" . lsp-workspace-restart))
  :custom
  (gc-cons-threshold 100000000)
  (lsp-auto-configure nil)
  (lsp-auto-execute-action nil)
  (lsp-auto-guess-root nil)
  (lsp-before-save-edits nil)
  (lsp-client-packages nil)
  (lsp-completion-provider :capf)
  (lsp-diagnostics-provider :flycheck)
  (lsp-document-sync-method 'incremental)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-render-all nil)
  (lsp-enable-completion-at-point t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding t)
  (lsp-enable-imenu t)
  (lsp-enable-indentation t)
  (lsp-enable-links t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-snippet t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color t)
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  (lsp-idle-delay 0.5)
  (lsp-modeline-code-actions-segments '(count icon name))
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-prefer-flymake nil)
  (lsp-response-timeout 10)
  (lsp-restart 'interactive)
  (lsp-semantic-tokens-enable t)
  (lsp-signature-render-documentation t)
  (read-process-output-max (* 1024 1024))
  :config
  ;;TODO: play with lsp-keymap-prefix
  (lsp-lens-mode 1)
  (lsp-headerline-breadcrumb-mode)
  (lsp-modeline-diagnostics-mode)
  (delight 'lsp-lens-mode " Ꙫ" 'lsp-lens))

(use-package lsp-ui
  :after lsp-mode
  :hook
  (lsp-mode-hook . lsp-ui-mode))

(use-package lsp-ui-doc
  :ensure lsp-ui
  :preface
  (defun custom/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :bind
  (:map lsp-mode-map
        ("C-c h" . lsp-ui-doc-glance))
  (:map mode-specific-map
        ("D" . custom/toggle-lsp-ui-doc))
  (:map goto-map
        ("I" . lsp-ui-doc-focus-frame))
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-delay 2)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil))

(use-package lsp-ui-peek
  :ensure lsp-ui
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-peek-fontify 'on-demand)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-peek-height 20))

(use-package lsp-ui-flycheck ;TODO: review lsp-ui-flycheck.el -> /nix/store/paw7y6dsyjsp9qjdf458sbjd9cmxsb6v-emacs-lsp-ui-20210216.1218/share/emacs/site-lisp/elpa/lsp-ui-20210216.1218/lsp-ui-flycheck.el
  :ensure lsp-ui
  :custom
  (lsp-ui-flycheck-enable t))

(use-package lsp-ui-imenu
  :ensure lsp-ui
  :hook
  (lsp-after-open-hook . lsp-enable-imenu)
  :bind
  (:map goto-map
        ("i" . lsp-ui-imenu))
  :custom
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'left)
  ;; lsp-ui-imenu-window-width set window width
  ;; lsp-ui-imenu--custom-mode-line-format mode line format
  ;; lsp-ui-imenu-auto-refresh auto refresh when necessary
  ;; lsp-ui-imenu-refresh-delay delay to refresh imenu
  )

(use-package lsp-ui-sideline
  :ensure lsp-ui
  :custom
  (lsp-ui-sideline-code-actions-prefix "")
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-delay 1))


;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;; (lsp-ui-peek-jump-backward)
;; (lsp-ui-peek-jump-forward)
;; (lsp-ui-peek-find-workspace-symbol "pattern 0")
;; ;; If the server supports custom cross references
;; (lsp-ui-peek-find-custom 'base "$cquery/base")

;; TODO: enable after proper setup
;; https://github.com/emacs-lsp/dap-mode#configuration
(use-package dap-mode
  :after lsp-mode
  :disabled
  ;TODO: package "ptvsd" for python
  :bind
  (:map dap-mode-map
        ;; example bindings
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle)))
  (dap-mode 1)
  (dap-ui-mode 1))

(use-package lsp-treemacs
  :after treemacs lsp-mode
  :bind
  (:map custom-lsp-treemacs-map
        ("e" . lsp-treemacs-errors-list)
        ("s" . lsp-treemacs-symbols))
  :config
  (lsp-treemacs-sync-mode 1))
