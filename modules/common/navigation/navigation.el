(use-package ace-window
  :after avy
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-background nil)
  (aw-leading-char-style 'char)
  (aw-scope 'visible)
  :config
  (ace-window-display-mode 1)
  :custom-face (aw-leading-char-face
                ((t (:inherit ace-jump-face-foreground
                              :foreground "green"
                              :height 0.1)))))

(use-package avy
  :bind
  ("C-:" . avy-goto-char)
  (:prefix-map custom-goto-map
               :prefix "M-s"
               ("M-s" . avy-goto-word-0))
  :custom
  (avy-timeout-seconds 0.5)
  (avy-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :custom-face (avy-goto-char-timer-face ((nil (:foreground "green" :weight bold))))
  :config
  (avy-setup-default))

(use-package avy-zap
  :bind
  ([remap zap-to-char] . avy-zap-to-char-dwim))

(use-package helm
  :bind
  ("<f10>" . helm-resume)
  (:prefix-map custom-nav-map
               :prefix "C-q"
               ("y" . helm-show-kill-ring)
               ("c" . helm-complex-command-history))
  (:prefix-map custom-help-map
               :prefix "<f1>"
               ("l" . helm-locate-library)
               ("i" . helm-info-lookup-symbol))
  (:map mode-specific-map
        ("C-SPC" . helm-mark-ring)
        ("l" . helm-locate))
  (:map help-map
        ("l" . helm-locate-library))
  (:map ctl-x-map
        ("C-r" . helm-recentf)
        ("b" . helm-mini))
  :custom
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-locate-fuzzy-match t)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (use-package helm-buffers)
  (use-package helm-elisp)
  (use-package helm-for-files)
  (use-package helm-locate)
  (use-package helm-ring))

(use-package helm-swoop ;TODO: compare with swoop
  :after helm
  :bind
  ("C-s" . helm-swoop)
  ([remap isearch-forward] . helm-swoop)
  :custom
  ;TODO: make some kind of switch for this case, as it was for ivy/swiper
  (helm-swoop-pre-input-function (lambda ())))

(use-package helm-descbinds
  :after helm
  :bind
  (:map custom-help-map
               ("b" . helm-descbinds))
  :config
  (helm-descbinds-mode))

(use-package helm-describe-modes
  :bind
  (:map ctl-x-map
        ("m" . helm-describe-modes))
  ([remap describe-mode] . helm-describe-modes)
  :config
  (helm-descbinds-mode))

(use-package helm-projectile
  :after (helm projectile)
  :preface
  (defun helm/combined ()
    "Preconfigured `helm'."
    (interactive)
    (condition-case nil
        (if (projectile-project-root)
            (helm-projectile)
          ;; otherwise fallback to `helm-mini'
          (helm-mini))
      ;; fall back to helm mini if an error occurs (usually in `projectile-project-root')
      (error (helm-mini))))
  :bind
  ("C-<f1>" . helm-projectile-switch-project)
  (:map custom-nav-map
               ("g" . helm-projectile-rg))
  (:map custom-projectile-map
               ("h" . helm/combined))
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile)
  (setq helm-projectile-fuzzy-match nil))

(use-package helm-tramp
  :hook
  (helm-tramp-pre-command-hook . (lambda ()
                                   (setq make-backup-files nil)
                                   (global-aggressive-indent-mode 0)
                                   (projectile-mode 0)
                                   (editorconfig-mode 0)))
  (helm-tramp-quit-hook . (lambda ()
                               (setq make-backup-files t)
                               (projectile-mode 1)
                               (editorconfig-mode 1)))
  :config
  (setenv "SHELL" "@bashExecutable@")
  :custom
  (tramp-default-method "ssh")
  (helm-tramp-docker-user "@mainUserName@"))

(use-package dired
  :commands dired
  :hook (dired-mode-hook . auto-revert-mode)
  :bind
  ([remap list-directory] . dired)
  (:map dired-mode-map
        ("e" . (lambda ()
                 (interactive)
                 (when (derived-mode-p 'dired-mode)
                   (if (file-directory-p (dired-get-filename))
                       (message "Directories cannot be opened in EWW")
                     (eww-open-file (dired-get-file-for-visit)))))))
  :preface
  (defun custom/revert-dired-buffer (func &rest args) (revert-buffer))
  :custom
  (dired-recursive-deletes 'top) ;; Allows recursive deletes
  (dired-dwim-target t)
  (dired-listing-switches "-lah1v --group-directories-first") ;;TODO: think of using TIME_STYLE env var
  :config
  (use-package dired-x)
  (put 'dired-find-alternate-file 'disabled nil)
  (advice-add 'dired-do-rename :after #'custom/revert-dired-buffer)
  (advice-add 'dired-create-directory :after #'custom/revert-dired-buffer)
  (use-package dired-filetype-face))

(use-package wdired
  :after dired
  :custom
  (wdired-allow-to-change-permissions 'advanced)
  :config
  (advice-add 'wdired-abort-changes :after #'custom/revert-dired-buffer))

(use-package dired-hide-dotfiles
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

;; https://github.com/emacs-helm/helm-navi

(use-package frame
  :preface
  (defvar opacity-percent 75 "Opacity percent")
  (defun custom/toggle-transparency ()
    (interactive)
    (let ((alpha (frame-parameter nil 'alpha)))
      (set-frame-parameter
       nil 'alpha
       (if (eql (cond ((numberp alpha) alpha)
                      ((numberp (cdr alpha))
                       (cdr alpha))
                      ;; Also handle undocumented (<active> <inactive>) form.
                      ((numberp (cadr alpha)) (cadr alpha)))
                100)
           `(,opacity-percent . 50) '(100 . 100)))))
  :bind
  (:prefix-map frame-map
               :prefix "<f2>"
               ("n" . make-frame-command)
               ("k" . delete-frame)
               ("s" . delete-other-frames)
               ("v" . custom/toggle-transparency))
  :config
  (add-to-list 'default-frame-alist `(alpha . (100 . 100)))
  (blink-cursor-mode 0)
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (setq frame-title-format "emacs - %b %f") ;; for various external tools
  (setq opacity-percent 75)
  (setq truncate-partial-width-windows nil))

(use-package imenu-anywhere
  :after helm
  :commands helm-imenu-anywhere
  :bind
  (:map custom-nav-map
               ("I" . helm-imenu-anywhere)))

;TODO: setup xref package itself
(use-package helm-xref
  :after helm
  :custom
  (xref-show-xrefs-function #'helm-xref-show-xrefs "Use Helm to show xrefs"))

(use-package helm-c-yasnippet
  :after (helm yasnippet)
  :bind
  (:map custom-yasnippet-map
        ("i" . helm-yas-complete))
  :custom
  (helm-yas-space-match-any-greedy t))

(use-package link-hint
  :demand t
  :bind
  (:map mode-specific-map
        :prefix-map link-hint-keymap
        :prefix "o"
        ("f" . link-hint-open-link)
        ("y" . link-hint-copy-link)
        ("F" . link-hint-open-multiple-links)
        ("Y" . link-hint-copy-multiple-links))
  :custom
  (link-hint-avy-style 'de-bruijn))

(use-package phi-search
  :hook (isearch-mode-hook . phi-search-from-isearch-mc/setup-keys)
  :config
  (use-package phi-search-mc
    :config
    (phi-search-mc/setup-keys)))

(use-package projectile
  :delight " prj"
  :bind
  (:prefix-map custom-projectile-map
               :prefix "<f8>"
               ("i" . projectile-invalidate-cache)
               ("k" . projectile-kill-buffers)
               ("C" . projectile-commander)
               ("d" . projectile-dired)
               ("f" . projectile-recentf)
               ("h" . helm-projectile-find-file))
  :custom
  (projectile-enable-caching t)
  (projectile-require-project-root nil)
  (projectile-completion-system 'helm)
  (projectile-track-known-projects-automatically t)
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))
  :hook
  (after-init-hook . projectile-mode))

(use-package rg
  :after helm
  :bind
  (:map custom-nav-map
        ("r" . rg)
        ("d" . rg-project))
  :custom
  (rg-group-result t)
  (rg-show-columns t)
  (rg-hide-command t)
  (rg-align-position-numbers t)
  (rg-align-line-number-field-length 3)
  (rg-align-column-number-field-length 3)
  (rg-align-line-column-separator "|")
  (rg-align-position-content-separator "|")
  :config
  (rg-define-toggle "--context 3" (kbd "C-c c")))

(define-hostmode poly-nix-hostmode :mode 'nix-mode)

(define-innermode poly-emacs-innermode
  :mode 'emacs-lisp-mode
  :head-matcher (rx "(use-package" space (zero-or-more alnum))
  :tail-matcher (rx ")")
  :head-mode 'host
  :tail-mode 'host)
(define-innermode poly-haskell-innermode
  :mode 'haskell-mode
  :head-matcher (rx (zero-or-more space) (or (minimal-match "--") "module" "{-# LANGUAGE"))
  :tail-matcher (rx ")")
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-nix-emacs-mode
  :hostmode 'poly-nix-hostmode
  :innermodes '(poly-emacs-innermode))
(define-polymode poly-nix-haskell-mode
  :hostmode 'poly-nix-hostmode
  :innermodes '(poly-haskell-innermode))
