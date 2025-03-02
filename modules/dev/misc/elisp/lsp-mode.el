;; TODO: investigate interoperation between lsp* and corfu/company, as found in some configs

(use-package lsp-mode
  :preface
  (defun custom/fix-completion-default ()
    (setq-local completion-category-defaults
                (assoc-delete-all 'lsp-capf completion-category-defaults)))
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration)
  (lsp-mode-hook . lsp-diagnostics-mode)
  (lsp-completion-mode-hook . custom/fix-completion-default)
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
  (lsp-eldoc-render-all t)
  (lsp-enable-completion-at-point t)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding t)
  (lsp-enable-imenu t)
  (lsp-enable-indentation t)
  (lsp-enable-links t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet nil)
  (lsp-enable-suggest-server-download nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color t)
  (lsp-enable-xref t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  (lsp-idle-delay 0.5)
  (lsp-keep-workspace-alive t)
  (lsp-keymap-prefix "C-c l")
  (lsp-lens-place-position 'above-line)
  (lsp-log-io t)
  (lsp-modeline-code-actions-segments '(count icon name))
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-prefer-flymake nil)
  (lsp-response-timeout 10)
  (lsp-restart 'interactive)
  (lsp-semantic-tokens-enable t)
  (lsp-session-file (expand-file-name (format "%s/.lsp-session-v1" no-littering-var-directory) user-emacs-directory))
  (lsp-signature-render-documentation t)
  (read-process-output-max (* 1024 1024))
  :config
  (use-package lsp-lens
    :delight " Ꙫ"
    :custom
    (lsp-lens-enable t)
    :config
    (lsp-lens-mode 1))
  (use-package lsp-dired
    :config
    (lsp-dired-mode 1))
  (use-package lsp-headerline
    :custom
    (lsp-headerline-breadcrumb-enable nil)
    (lsp-headerline-breadcrumb-enable-diagnostics nil)
    (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
    (lsp-headerline-breadcrumb-icons-enable nil)
    :config
    (lsp-headerline-breadcrumb-mode 1))
  (use-package lsp-modeline
    :custom
    (lsp-modeline-code-actions-enable nil)
    (lsp-modeline-diagnostics-enable nil)
    (lsp-modeline-workspace-status-enable nil)
    (lsp-signature-doc-lines 1)
    :config
    (lsp-modeline-diagnostics-mode))
  (use-package lsp-completion
    :custom
    (lsp-completion-enable t)
    (lsp-completion-enable-additional-text-edit t)
    (lsp-enable-snippet nil)
    (lsp-completion-show-kind t)))

(use-package lsp-ui
  :after lsp-mode
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-delay 0.5))

(use-package lsp-ui-doc
  :after lsp-ui
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
  (:map custom-goto-map
        ("I" . lsp-ui-doc-focus-frame))
  :custom-face
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
  :after lsp-ui
  :bind
  (:map lsp-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-peek-fontify 'on-demand)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-peek-height 20))

(use-package lsp-ui-flycheck
  :after lsp-ui
  :bind
  (:map mode-specific-map
        ("!" . lsp-ui-flycheck-list))
  :custom
  (lsp-ui-flycheck-enable t))

(use-package lsp-ui-sideline
  :after lsp-mode
  :after lsp-ui
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-sideline-code-actions-prefix "💡 ")
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-delay 1))

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

(with-eval-after-load 'cape
  (with-eval-after-load 'lsp-mode
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)))
