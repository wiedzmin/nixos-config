(use-package webpaste
  :bind
  (:map custom-webpaste-map
        ("b" . webpaste-paste-buffer)
        ("r" . webpaste-paste-region))
  :custom
  (webpaste-provider-priority '("ix.io" "gist.github.com")))

(use-package jinja2-mode
  :mode "\\.j2$")

(use-package yaml-mode
  :bind
  (:map yaml-mode-map
        ("C-m" . newline-and-indent)
        ("<return>" . newline-and-indent)))

(use-package fic-mode
  :hook
  (prog-mode . fic-mode))

(use-package elmacro)

(use-package comby
  :custom
  (comby-args '("-exclude" "@combyExcludes@")))

(use-package lsp-mode
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration)
  :bind
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
  :preface
  (defun custom/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  (lsp-after-open-hook . lsp-enable-imenu)
  :bind
  (:map lsp-mode-map
        ("C-c h" . lsp-ui-doc-glance))
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  (:map mode-specific-map
        ("R" . lsp-workspace-restart)
        ("D" . custom/toggle-lsp-ui-doc))
  (:map goto-map
        ("I" . lsp-ui-doc-focus-frame)
        ("i" . lsp-ui-imenu))
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-header t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'left)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-code-actions-prefix ""))

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

(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path "@plantumlJar@")
  (org-plantuml-jar-path plantuml-jar-path)
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))

(use-package blockdiag-mode)
