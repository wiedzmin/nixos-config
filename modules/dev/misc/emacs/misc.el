(use-package webpaste
  :bind
  (:map mode-specific-map
        :prefix-map custom-webpaste-map
        :prefix "["
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
  :preface
  (defvar lsp-on-touch-time 0)
  (defun custom/lsp-on-change (func &rest args)
    ;; don't run `lsp-on-change' too frequently
    (when (> (- (float-time (current-time))
                lsp-on-touch-time) 30) ;; 30 seconds
      (setq lsp-on-touch-time (float-time (current-time)))
      (funcall func args)))
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration)
  :bind
  (:map lsp-mode-map
        ("C-M-r" . lsp-rename)
        ("C-c h" . lsp-ui-doc-glance)
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (read-process-output-max (* 1024 1024))
  (gc-cons-threshold 100000000)
  (lsp-completion-provider :capf)
  (lsp-enable-file-watchers nil)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-auto-guess-root nil)
  (lsp-before-save-edits nil)
  (lsp-document-sync-method 'incremental)
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-highlight-symbol-at-point nil)
  (lsp-inhibit-message t)
  (lsp-diagnostics-provider :flycheck)
  (lsp-message-project-root-warning t)
  (lsp-prefer-flymake nil)
  (lsp-response-timeout 20)
  (lsp-enable-folding nil)
  (lsp-enable-completion-at-point t)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-links nil)  ;?
  (lsp-restart 'auto-restart)
  (lsp-client-packages nil)
  (lsp-idle-delay 0.1)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 32768)
  :config
  ;;TODO: play with lsp-keymap-prefix
  (lsp-lens-mode 1)
  (delight 'lsp-lens-mode " Ꙫ" 'lsp-lens)
  (advice-add 'lsp-on-change :around 'custom/lsp-on-change))

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
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  (:map mode-specific-map
        ("R" . lsp-workspace-restart)
        ("D" . custom/toggle-lsp-ui-doc))
  (:map custom-goto-map
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
  (:map mode-specific-map
        :prefix-map custom-lsp-treemacs-map
        :prefix "t"
        ("e" . lsp-treemacs-errors-list)
        ("s" . lsp-treemacs-symbols))
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :bind
  (:map mode-specific-map
        ("o b" . lsp-ivy-workspace-symbol)
        ("o g" . lsp-ivy-global-workspace-symbol))
  (:map lsp-mode-map
        ([remap xref-find-apropos] . lsp-ivy-global-workspace-symbol))
  (:map custom-nav-map
        ("s" . lsp-ivy-global-workspace-symbol)))
