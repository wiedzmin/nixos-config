(use-package webpaste
  :ensure t
  :bind
  (:prefix-map custom-webpaste-map
               :prefix "M-p"
               ("b" . webpaste-paste-buffer)
               ("r" . webpaste-paste-region))
  :custom
  (webpaste-provider-priority '("ix.io" "gist.github.com")))

(use-package jinja2-mode
  :ensure t
  :mode "\\.j2$")

(use-package yaml-mode
  :mode "\\.yml\\'"
  :quelpa
  (yaml-mode :repo "yoshiki/yaml-mode" :fetcher github :version original))

(use-package diff-hl
  :ensure t
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (org-mode-hook . diff-hl-mode)
  (prog-mode . diff-hl-mode)
  :config
  (diff-hl-margin-mode 1)
  (diff-hl-amend-mode 1)
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode 1))

(use-package diff-mode
  :mode "diff")

(use-package fic-mode
  :ensure t
  :hook
  (prog-mode . fic-mode))

(use-package multi-compile :ensure t)

(use-package company-restclient
  :ensure t
  :after company restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-lsp
  :ensure t
  :after lsp-ui
  :custom
  (company-lsp-async t)
  (company-lsp-cache-candidates 'auto)
  (company-lsp-enable-recompletion t)
  (company-lsp-enable-snippet t)
  :config
  (push 'company-lsp company-backends))

(use-package company-tabnine
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . company-mode)
  :bind
  (:map lsp-mode-map
        ("C-M-r" . lsp-rename))
  :custom
  (lsp-auto-guess-root t)
  (lsp-before-save-edits t)
  (lsp-document-sync-method 'incremental)
  (lsp-eldoc-render-all nil)
  (lsp-highlight-symbol-at-point nil)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-prefer-flymake nil)
  (lsp-response-timeout 20)
  :config
  (use-package lsp-clients))

(use-package lsp-ui
  :ensure t
  :after lsp-mode avy
  :preface
                                        ; TODO: bind to key
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
        ("R" . lsp-restart-workspace))
  (:map custom-goto-map
        ("i" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'left)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-code-actions-prefix ""))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode avy
  :bind
  (:map mode-specific-map
        ("o b" . lsp-ivy-workspace-symbol)
        ("o g" . lsp-ivy-global-workspace-symbol))
  (:map lsp-mode-map
        ([remap xref-find-apropos] . lsp-ivy-global-workspace-symbol)))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package elmacro
  :ensure t)
