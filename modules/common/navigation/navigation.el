(use-package ace-link
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("l" . counsel-ace-link))
  :config
  (ace-link-setup-default))

(use-package ace-window
  :after avy
  :commands ace-window
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

;TODO: play  with groups/workspaces
(use-package bufler
  :quelpa
  (bufler :repo "alphapapa/bufler.el" :fetcher github)
  :bind
  ("C-x C-b" . bufler-list))

(use-package avy
  :demand t
  :bind
  (("C-:" . avy-goto-char)
   :prefix-map custom-goto-map
   :prefix "M-s"
   ("M-s" . avy-goto-word-0)
   ("M-a" . swiper-avy))
  :custom
  (avy-timeout-seconds 0.5)
  (avy-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :custom-face (avy-goto-char-timer-face ((nil (:foreground "green" :weight bold))))
  :config
  (avy-setup-default))

(use-package avy-zap
  :bind
  ([remap zap-to-char] . avy-zap-to-char-dwim))

(use-package counsel
  :delight counsel-mode
  :init
  (require 'iso-transl)
  :bind
  (([remap menu-bar-open] . counsel-tmm)
   ([remap insert-char] . counsel-unicode-char)
   ([remap isearch-forward] . counsel-grep-or-swiper)
   ("C-h L" . counsel-locate)
   :prefix-map custom-counsel-map
   :prefix "<f9>"
   ("y" . counsel-yank-pop)
   ("m" . counsel-mark-ring)
   ("c" . counsel-command-history)
   ("e" . counsel-expression-history)
   ("p" . counsel-package)
   ("l" . counsel-git-log)
   ("g" . counsel-rg)
   ("G" . (lambda () (interactive) (counsel-rg (thing-at-point 'symbol))))
   ("I" . ivy-imenu-anywhere)
   :prefix-map custom-help-map
   :prefix "<f1>"
   ("l" . counsel-find-library)
   ("b" . counsel-descbinds)
   ("i" . counsel-info-lookup-symbol)
   :map mode-specific-map
   ("C-SPC" . counsel-mark-ring)
   ("C-." . counsel-fzf)
   ("w" . counsel-wmctrl)
   :map ctl-x-map
   ("C-r" . counsel-recentf)
   ("m" . counsel-minor)
   :map help-map
   ("l" . counsel-find-library)
   :map iso-transl-ctl-x-8-map
   ("RET" . counsel-unicode-char)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :custom
  (counsel-git-cmd "rg --files")
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  :config
  (counsel-mode 1))

(use-package counsel-projectile
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
    (let ((current-project-name (projectile-default-project-name (locate-dominating-file buffer-file-name ".git"))))
      (aif (get-buffer (concat "magit: " current-project-name))
          (switch-to-buffer it)
        (magit-status))))
  (defun custom/ensure-project-switch-buffer (arg)
    "Custom switch to buffer.
         With universal argument ARG or when not in project, rely on
         `ivy-switch-buffer'.
         Otherwise, use `counsel-projectile-switch-to-buffer'."
    (interactive "P")
    (if (or arg
            (not (projectile-project-p)))
        (ivy-switch-buffer)
      (counsel-projectile-switch-to-buffer)))
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
  (:prefix-map custom-projectile-map
               :prefix "<f8>"
               ("i" . projectile-invalidate-cache)
               ("k" . projectile-kill-buffers)
               ("C" . projectile-commander)
               ("d" . projectile-dired)
               ("f" . projectile-recentf)
               ("t" . custom/open-project-todos)
               ("m" . custom/open-project-magit-status)
               ("T" . doom/ivy-tasks)
               ("h" . projectile-find-file)
               ("c" . projectile-codesearch-search))
  (:map ctl-x-map
        ("j j" . counsel-projectile-switch-project)
        ("b" . custom/ensure-project-switch-buffer))
  :config
  (counsel-projectile-mode 1)
  (add-to-list 'counsel-projectile-switch-project-action
               '("c" counsel-projectile-switch-project-action-codesearch-search "search project's files with Codesearch") t)
  (add-to-list 'counsel-projectile-switch-project-action
               '("t" counsel-projectile-switch-project-action-open-todos "open project's todos") t)
  (add-to-list 'counsel-projectile-switch-project-action
               '("m" counsel-projectile-switch-project-action-open-magit-status "open project's magit status buffer") t)
  (setq projectile-switch-project-action 'counsel-projectile-switch-project))

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
  (("M-o" . ace-window)
   :prefix-map frame-map
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

(use-package ivy
  :delight ivy-mode
  :bind
  (("M-<f12>" . counsel-switch-buffer)
   ("<f10>" . ivy-resume)
   :map ctl-x-map
   ("b" . counsel-switch-buffer)
   :map mode-specific-map
   ("v" . ivy-push-view)
   ("V" . ivy-pop-view)
   :map ivy-minibuffer-map
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
     (t . ivy--regex-ignore-order)))
  )

(use-package imenu-anywhere
  :commands ivy-imenu-anywhere)

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
  :demand t
  :bind
  (:map mode-specific-map
        :prefix-map link-hint-keymap
        :prefix "o"
        ("s" . custom/open-url-current-buffer)
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
  :delight (projectile-mode " prj")
  :custom
  (projectile-enable-caching t)
  (projectile-require-project-root nil)
  (projectile-completion-system 'ivy)
  (projectile-track-known-projects-automatically t)
  (projectile-switch-project-action 'projectile-commander)
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))
  :hook
  (after-init-hook . projectile-mode))

(use-package rg
  :bind
  (:map mode-specific-map
        ("r" . rg)
        ("d" . rg-project)
        ("m" . rg-dwim))
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
  :commands swiper swiper-multi
  :bind
  (("C-s" . swiper)
   ("C-S-s" . swiper-thing-at-point)
   :map custom-counsel-map
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
