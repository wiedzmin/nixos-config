(use-package ace-link
  :after link-hint
  :bind
  (:map link-hint-keymap
        ;; This is fallback option for link-hint itself
        ;; because link-hint is spuriously stops working
        ;; between package upgrades.
        ("l" . ace-link-org))
  :config
  (ace-link-setup-default))

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

(use-package ivy
  :delight
  :bind
  ("M-<f12>" . counsel-switch-buffer)
  ("<f10>" . ivy-resume)
  (:map ctl-x-map
        ("b" . counsel-switch-buffer))
  (:map mode-specific-map
        ("v" . ivy-push-view)
        ("V" . ivy-pop-view))
  (:map ivy-minibuffer-map
        ("C-j" . ivy-immediate-done))
  :config
  (ivy-mode 1)
  :custom-face
  (ivy-current-match ((t (:background "gray1"))))
  :custom
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  (ivy-use-virtual-buffers t) ;; add 'recentf-mode’and bookmarks to 'ivy-switch-buffer'.
  (ivy-height @ivyCandidatesCount@) ;; number of result lines to display
  (ivy-initial-inputs-alist nil) ;; no regexp by default
  (ivy-re-builders-alist
   '((read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy--regex-ignore-order))))

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
  (avy-setup-default)
  (use-package ivy-avy))

(use-package avy-zap
  :bind
  ([remap zap-to-char] . avy-zap-to-char-dwim))

(use-package counsel
  :delight
  :init
  (require 'iso-transl)
  :bind
  ([remap menu-bar-open] . counsel-tmm)
  ([remap insert-char] . counsel-unicode-char)
  ([remap isearch-forward] . counsel-grep-or-swiper)
  (:prefix-map custom-nav-map
               :prefix "C-q"
               ("y" . counsel-yank-pop)
               ("m" . counsel-mark-ring)
               ("c" . counsel-command-history)
               ("l" . counsel-git-log)
               ("g" . counsel-rg)
               ("G" . (lambda () (interactive) (counsel-rg (thing-at-point 'symbol))))
               ("I" . ivy-imenu-anywhere))
  (:prefix-map custom-help-map
               :prefix "<f1>"
               ("l" . counsel-find-library)
               ("b" . counsel-descbinds)
               ("i" . counsel-info-lookup-symbol))
  (:map mode-specific-map
        ("C-SPC" . counsel-mark-ring)
        ("l" . counsel-locate))
  (:map help-map
        ("l" . counsel-find-library))
  (:map ctl-x-map
        ("C-r" . counsel-recentf)
        ("m" . counsel-minor))
  (:map iso-transl-ctl-x-8-map
        ("RET" . counsel-unicode-char))
  :custom
  (counsel-git-cmd "rg --files")
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  :config
  (counsel-mode 1))

(use-package counsel-tramp
  :hook
  (counsel-tramp-pre-command-hook . (lambda ()
                                      (setq make-backup-files nil)
                                      (global-aggressive-indent-mode 0)
                                      (projectile-mode 0)
                                      (editorconfig-mode 0))) ;TODO: check if EC is enabled
  (counsel-tramp-quit-hook . (lambda ()
                               (projectile-mode 1)
                               (editorconfig-mode 1))) ;TODO: check if EC is enabled
  :config
  (setenv "SHELL" "@bashExecutable@")
  :custom
  (tramp-default-method "ssh")
  (counsel-tramp-docker-user "@mainUserName@"))

(use-package counsel-jq)

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

(use-package doom-todo-ivy
  :quelpa
  (doom-todo-ivy :repo "jsmestad/doom-todo-ivy" :fetcher github)
  :commands doom/ivy-tasks)

(use-package frame
  :preface
  (defvar custom-frame-title-format '(:eval
   (format "%s@%s: %s %s"
           (or (file-remote-p default-directory 'user)
               user-real-login-name)
           (or (file-remote-p default-directory 'host)
               system-name)
           (buffer-name)
           (cond
            (buffer-file-truename
             (concat "(" buffer-file-truename ")"))
            (dired-directory
             (concat "{" dired-directory "}"))
            (t
             "[no file]")))))
  (defun keep-custom-frame-title (window)
    (setq-default frame-title-format custom-frame-title-format))
  (defun mark-done-kill-frame ()
    (interactive)
    (server-edit)
    (delete-frame))
  :bind
  (:prefix-map frame-map
               :prefix "<f2>"
               ("n" . make-frame-command)
               ("k" . mark-done-kill-frame)
               ("s" . delete-other-frames))
  (:map ctl-x-map
        ("C-c" . delete-frame)) ;; for keeping daemon running
  :config
  (unless (string-equal "i3" (getenv "CURRENT_WM"))
    (add-hook 'pre-redisplay-functions 'keep-custom-frame-title))
  (blink-cursor-mode 0)
  (setq-default frame-title-format custom-frame-title-format) ;; for various external tools
  (setq truncate-partial-width-windows nil))

(use-package imenu-anywhere
  :commands ivy-imenu-anywhere
  :bind
  (:map custom-nav-map
               ("I" . ivy-imenu-anywhere)))

(use-package ivy-historian
  :after ivy
  :config
  (ivy-historian-mode))

(use-package ivy-rich
  :after ivy
  :defines ivy-rich-abbreviate-paths ivy-rich-switch-buffer-name-max-length
  :custom
  (ivy-rich-switch-buffer-name-max-length 60 "Increase max length of buffer name.")
  :config
  (ivy-rich-mode 1))

;TODO: setup xref package itself
(use-package ivy-xref
  :after ivy
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs "Use Ivy to show xrefs"))

(use-package ivy-yasnippet
  :after (ivy yasnippet)
  :bind
  (:map custom-yasnippet-map
        ("i" . ivy-yasnippet)))

(use-package link-hint
  :bind
  (:map mode-specific-map
        :prefix-map link-hint-keymap
        :prefix "o"
        ; ("s" . custom/open-url-current-buffer)
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
  :delight
  :bind
  (:prefix-map custom-projectile-map
               :prefix "<f8>"
               ("i" . projectile-invalidate-cache)
               ("k" . projectile-kill-buffers)
               ("C" . projectile-commander)
               ("d" . projectile-dired)
               ("f" . projectile-recentf))
  :custom
  (projectile-enable-caching t)
  (projectile-require-project-root t)
  (projectile-completion-system 'ivy)
  (projectile-track-known-projects-automatically t)
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-bottom-up
     projectile-root-top-down
     projectile-root-top-down-recurring))
  :hook
  (after-init-hook . projectile-mode)
  :config
  :delight '(:eval (concat " ¶[" (projectile-project-name) "]")))

(use-package counsel-projectile
  :after (counsel projectile)
  :preface
  (defun custom/open-project-todos ()
    (interactive)
    (let ((todos-file (expand-file-name "todo.org" (projectile-project-root))))
      (condition-case nil
          (when (file-exists-p todos-file)
            (find-file todos-file))
        (error (message "Cannot find project todos")))))
  (defun custom/open-project-magit-status ()
    (interactive)
    (require 'anaphora)
    (let ((current-project-name (projectile-default-project-name (locate-dominating-file buffer-file-name ".git"))))
      (aif (get-buffer (concat "magit: " current-project-name))
          (switch-to-buffer it)
        (magit-status))))
  (defun counsel-projectile-switch-project-action-codesearch-search (project)
    "Search project's files with Codesearch."
    (let ((projectile-switch-project-action #'projectile-codesearch-search))
      (counsel-projectile-switch-project-by-name project)))
  (defun counsel-projectile-switch-project-action-open-todos (project)
    "Open project's TODOs."
    (let ((projectile-switch-project-action #'custom/open-project-todos))
      (counsel-projectile-switch-project-by-name project)))
  (defun counsel-projectile-switch-project-action-open-magit-status (project)
    "Open project's Magit status buffer."
    (let ((projectile-switch-project-action #'custom/open-project-magit-status))
      (counsel-projectile-switch-project-by-name project)))
  :bind
  ("C-<f1>" . counsel-projectile-switch-project)
  (:map custom-projectile-map
        ("t" . custom/open-project-todos)
        ("m" . custom/open-project-magit-status)
        ("T" . doom/ivy-tasks)
        ("h" . counsel-projectile)
        ("c" . projectile-codesearch-search))
  :config
  (counsel-projectile-mode 1)
  (add-to-list 'counsel-projectile-switch-project-action
               '("c" counsel-projectile-switch-project-action-codesearch-search "search project's files with Codesearch") t)
  (add-to-list 'counsel-projectile-switch-project-action
               '("t" counsel-projectile-switch-project-action-open-todos "open project's todos") t)
  (add-to-list 'counsel-projectile-switch-project-action
               '("m" counsel-projectile-switch-project-action-open-magit-status "open project's magit status buffer") t)
  (setq projectile-switch-project-action 'counsel-projectile-switch-project))

(use-package rg
  :after counsel
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

(use-package swiper
  :after avy                            ;check
  :commands swiper swiper-multi
  :bind
  ("C-s" . swiper)
  ("C-S-s" . swiper-thing-at-point)
  (:map custom-nav-map
        ("M-a" . swiper-avy)
        ("m" . swiper-multi))
  :custom
  (swiper-include-line-number-in-search t)
  :custom-face (swiper-match-face-1 ((t (:background "#dddddd"))))
  :custom-face (swiper-match-face-2 ((t (:background "#bbbbbb" :weight bold))))
  :custom-face (swiper-match-face-3 ((t (:background "#bbbbff" :weight bold))))
  :custom-face (swiper-match-face-4 ((t (:background "#ffbbff" :weight bold)))))

(use-package avy-flycheck
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("e" . avy-flycheck-goto-error)))

(use-package block-nav
  :bind
  ("C-=" . block-nav-next-indentation-level)
  ("C--" . block-nav-previous-indentation-level)
  ("C-<down>" . block-nav-next-block)
  ("C-<up>" . block-nav-previous-block)
  :custom
  (block-nav-move-skip-shallower t)
  (block-nav-center-after-scroll t))

(use-package treemacs
  :bind
  (:map ctl-x-map
        :prefix-map custom-treemacs-map
        :prefix "t"
        ("1"   . treemacs-delete-other-windows)
        ("t"   . treemacs)
        ("B"   . treemacs-bookmark)
        ("C-t" . treemacs-find-file)
        ("M-t" . treemacs-find-tag))
  :custom
  (treemacs-collapse-dirs t)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-directory-name-transformer #'identity)
  (treemacs-display-in-side-window t)
  (treemacs-eldoc-display t)
  (treemacs-file-event-delay 5000)
  ;; (treemacs-file-extension-regex 'treemacs-last-period-regex-value)
  (treemacs-file-follow-delay 0.2)
  (treemacs-file-name-transformer #'identity)
  (treemacs-follow-after-init t)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-missing-project-action 'ask)
  (treemacs-move-forward-on-expand nil)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name
                          (format "%s/treemacs-persist" no-littering-var-directory) user-emacs-directory))
  (treemacs-position 'left)
  (treemacs-recenter-distance 0.1)
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-recenter-after-project-jump 'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-space-between-root-nodes nil)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-user-mode-line-format nil)
  (treemacs-user-header-line-format nil)
  (treemacs-width 35)
  (treemacs-workspace-switch-cleanup nil)
  (treemacs-python-executable "@python3Binary@")
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred))

(use-package treemacs-projectile
  :demand t
  :after treemacs projectile
  :bind
  (:map custom-projectile-map
        ("e" . treemacs-projectile)))

(use-package goggles
  :delight " 6d"
  :config
  (goggles-mode)
  (setq-default goggles-pulse t))

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

;; https://github.com/abo-abo/hydra
;; https://github.com/jerrypnz/major-mode-hydra.el
;; https://github.com/jeremyf/dotzshrc/blob/32be278ff00712e7061e36c105bd3b9bae23e933/emacs/jnf-org-roam.el#L106-L135
