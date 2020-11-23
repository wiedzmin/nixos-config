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

;TODO: investigate fringe updates lifecycle
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

(use-package company-restclient
  :after company restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-tabnine
  :after (company unicode-escape)
  :preface
  (defun tabnine/bury-company-lsp ()
    (when (memq 'company-lsp company-backends)
      (setq-local company-backends (-flatten (remove 'company-lsp company-backends)))))
  (defun company/sort-by-tabnine (candidates) ;; Integrate company-tabnine with lsp-mode
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6)))))
  :hook
  (lsp-after-open . (lambda ()
                      (setq company-tabnine-max-num-results 3)
                      (add-to-list 'company-transformers 'company/sort-by-tabnine t)
                      (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))))
  ;TODO: bind 'company-other-backend
  :custom
  (company-tabnine-max-num-results 9)
  :config
  (add-to-list 'company-backends #'company-tabnine)
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
  (lsp-idle-delay 0.1)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 32768)
  :config
  (use-package lsp-clients)
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
        ("R" . lsp-restart-workspace)
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
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-code-actions-prefix ""))

;; TODO: enable after proper setup
;; https://github.com/emacs-lsp/dap-mode#configuration
;; TODO: install gdb (requirement)
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

(when @direnvGranularityProject@
  (use-package direnv
    :config
    (direnv-mode)))

(when @direnvGranularityFile@
  (use-package envrc
    :quelpa
    (envrc :repo "purcell/envrc" :fetcher github)
    :config
    (envrc-global-mode)))

(use-package elmacro)

(use-package comby
  :quelpa
  (comby :repo "s-kostyaev/comby.el" :fetcher github)
  :custom
  (comby-args '("-exclude" "@combyExcludes@")))

(use-package nix-buffer)
