(use-package webpaste
  :bind
  (:prefix-map custom-webpaste-map
               :prefix "M-p"
               ("b" . webpaste-paste-buffer)
               ("r" . webpaste-paste-region))
  :custom
  (webpaste-provider-priority '("ix.io" "gist.github.com")))

(use-package jinja2-mode
  :mode "\\.j2$")

(use-package diff-hl
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (org-mode-hook . diff-hl-mode)
  (prog-mode . diff-hl-mode)
  :config
  (when (string-equal "i3" (getenv "CURRENT_WM"))
    (diff-hl-margin-mode 1)
    (diff-hl-flydiff-mode 1)
    (global-diff-hl-mode 1))
  (diff-hl-amend-mode 1))

(use-package fic-mode
  :hook
  (prog-mode . fic-mode))

(use-package yaml-mode
  :bind
  (:map yaml-mode-map
        ("C-m" . newline-and-indent)
        ("<return>" . newline-and-indent)))

(use-package multi-compile
  :custom
  (multi-compile-completion-system 'default))

(use-package company-restclient
  :after company restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-lsp
  :after lsp-ui
  :custom
  (company-lsp-async t)
  (company-lsp-cache-candidates 'auto)
  (company-lsp-enable-recompletion t)
  (company-lsp-enable-snippet t))

(use-package company-tabnine
  :after (company unicode-escape)
  :preface
  (defun tabnine/bury-company-lsp ()
    (when (memq 'company-lsp company-backends)
      (setq-local company-backends (-flatten (remove 'company-lsp company-backends)))))
  :config
  (advice-add 'lsp :after #'tabnine/bury-company-lsp))

(use-package lsp-mode
  :preface
  (defvar lsp-on-touch-time 0)
  (defun custom/lsp-on-change (func &rest args)
    ;; don't run `lsp-on-change' too frequently
    (when (> (- (float-time (current-time))
                lsp-on-touch-time) 30) ;; 30 seconds
      (setq lsp-on-touch-time (float-time (current-time)))
      (funcall func args)))
  :hook (lsp-mode . company-mode)
  :bind
  (:map lsp-mode-map
        ("C-M-r" . lsp-rename)
        ("C-c h" . lsp-ui-doc-glance))
  :custom
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-auto-guess-root t)
  (lsp-before-save-edits nil)
  (lsp-document-sync-method 'incremental)
  (lsp-eldoc-render-all nil)
  (lsp-highlight-symbol-at-point nil)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-prefer-flymake nil)
  (lsp-response-timeout 20)
  (lsp-enable-folding nil)
  (lsp-enable-completion-at-point nil)  ;?
  (lsp-enable-symbol-highlighting nil)  ;?
  (lsp-enable-links nil)  ;?
  (lsp-restart 'auto-restart)
  (lsp-client-packages nil)
  :config
  (use-package lsp-clients)
  (advice-add 'lsp-on-change :around 'custom/lsp-on-change))

(use-package lsp-ui
  :after lsp-mode avy
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
        ("R" . lsp-restart-workspace)
        ("D" . custom/toggle-lsp-ui-doc))
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

(use-package helm-lsp
  :after (lsp-mode helm)
  :bind
  (:map custom-nav-map
        ("s" . helm-lsp-global-workspace-symbol)))

(use-package direnv
  :config
  (direnv-mode))

(use-package elmacro)
