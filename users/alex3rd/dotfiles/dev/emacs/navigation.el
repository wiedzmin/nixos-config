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
  :general
  ("M-o" 'ace-window)
  (:prefix "<f2>"
           "n" 'make-frame-command
           "k" 'delete-frame
           "s" 'delete-other-frames
           "v" 'custom/toggle-transparency)
  :config
  (general-unbind 'global "M-o")
  (add-to-list 'default-frame-alist `(alpha . (100 . 100)))
  (blink-cursor-mode 0)
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (setq frame-title-format "emacs - %b %f") ;; for various external tools
  (setq opacity-percent 75)
  (setq truncate-partial-width-windows nil))

(use-package ibuffer
  :secret "ibuffer.el.gpg"
  :commands ibuffer
  :general
  ([remap list-buffers] 'ibuffer)
  (:keymaps 'ibuffer-mode-map
            "/ ." '(lambda (qualifier)
                     (interactive "sFilter by extname: ")
                     (ibuffer-filter-by-filename (concat "\\." qualifier "$")))
            "M-o" 'other-window) ; was ibuffer-visit-buffer-1-window
  :hook (ibuffer-mode-hook . (lambda ()
                               ;; Make sure we're always using our buffer groups
                               (ibuffer-switch-to-saved-filter-groups "default")))
  :custom
  (ibuffer-default-sorting-mode 'major-mode) ;recency
  (ibuffer-default-shrink-to-minimum-size t)
  (ibuffer-jump-offer-only-visible-buffers t)
  (ibuffer-saved-filters
   '(("dired" ((mode . dired-mode)))
     ("foss" ((filename . "foss")))
     ("pets" ((filename . "pets")))
     ("jabberchat" ((mode . jabber-chat-mode)))
     ("orgmode" ((mode . org-mode)))
     ("elisp" ((mode . emacs-lisp-mode)))
     ("fundamental" ((mode . fundamental-mode)))
     ("haskell" ((mode . haskell-mode))))))

(use-package ibuffer-project
  :ensure t
  :defer t
  :hook ((ibuffer-hook . (lambda () (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))))
         (ibuffer-hook . (lambda ()
                           (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                           (unless (eq ibuffer-sorting-mode 'project-file-relative)
                             (ibuffer-do-sort-by-project-file-relative)))))
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " project-file-relative))))

(use-package ivy
  :ensure t
  :delight ivy-mode
  :general
  ("M-<f12>" 'ivy-switch-buffer)
  ("<f10>" 'ivy-resume)
  (:keymaps 'ctl-x-map
            "b" 'ivy-switch-buffer)
  (:keymaps 'mode-specific-map
            "v" 'ivy-push-view
            "V" 'ivy-pop-view)
  (:keymaps 'ivy-minibuffer-map
            "C-j" 'ivy-immediate-done)
  :config
  (general-unbind 'global "C-x C-b")
  (ivy-mode 1)
  :custom-face
  (ivy-current-match ((t (:background "gray1"))))
  :custom
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  (ivy-use-virtual-buffers t) ;; add 'recentf-mode’and bookmarks to 'ivy-switch-buffer'.
  (ivy-height 20) ;; number of result lines to display
  (ivy-count-format "%d/%d ")
  (ivy-initial-inputs-alist nil) ;; no regexp by default
  (ivy-re-builders-alist
   ;; allow input not in order
   '((read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy--regex-ignore-order))))

(use-package counsel
  :ensure t
  :defer 2
  :after (swiper)
  :delight counsel-mode
  :preface
  (defun custom/open-org-file ()
    (interactive)
    (ivy-read "Org files: "
              (funcall #'(lambda () (f-files (at-org-dir) nil t)))
              :action #'(lambda (candidate)
                          (find-file candidate))
              :require-match t
              :caller 'custom/open-org-file))
  (defun custom/open-org-kb-file ()
    (interactive)
    (ivy-read "Org files: "
              (funcall #'(lambda () (f-files (at-org-kb-dir) nil t)))
              :action #'(lambda (candidate)
                          (find-file candidate))
              :require-match t
              :caller 'custom/open-org-kb-file))
  :init
  (require 'iso-transl)
  :general
  ([remap menu-bar-open] 'counsel-tmm)
  ([remap insert-char] 'counsel-unicode-char)
  ([remap isearch-forward] 'counsel-grep-or-swiper)
  (:keymaps 'mode-specific-map
            "C-SPC" 'counsel-mark-ring
            "C-." 'counsel-fzf
            "w" 'counsel-wmctrl
            "O" 'custom/open-org-file
            "K" 'custom/open-org-kb-file)
  (:keymaps 'ctl-x-map
            "C-r" 'counsel-recentf)
  ("C-h L" 'counsel-locate)
  (:prefix "<f9>"
           "y" 'counsel-yank-pop
           "m" 'counsel-mark-ring
           "c" 'counsel-command-history
           "e" 'counsel-expression-history
           "p" 'counsel-package
           "l" 'counsel-git-log
           "g" 'counsel-rg
           "G" '(lambda () (interactive) (counsel-rg (thing-at-point 'symbol)))
           "m" 'swiper-multi
           "I" 'ivy-imenu-anywhere)
  (:keymaps 'help-map
            "l" 'counsel-find-library)
  (:prefix "<f1>"
           "l" 'counsel-find-library
           "b" 'counsel-descbinds
           "i" 'counsel-info-lookup-symbol)
  (:keymaps 'iso-transl-ctl-x-8-map
            "RET" 'counsel-unicode-char)
  (:keymaps 'ivy-minibuffer-map
            "M-y" 'ivy-next-line)
  :custom
  (counsel-git-cmd "rg --files")
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :ensure t
  :after (ivy)
  :defines ivy-rich-abbreviate-paths ivy-rich-switch-buffer-name-max-length
  :custom
  (ivy-rich-switch-buffer-name-max-length 60 "Increase max length of buffer name.")
  :config
  (ivy-rich-mode 1))

(use-package ivy-xref
  :ensure t
  :defer t
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs "Use Ivy to show xrefs"))

(use-package ivy-dired-history
  :ensure t
  :after (dired savehist)
  :config
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))

(use-package ivy-historian
  :ensure t
  :config
  (ivy-historian-mode))

(use-package link-hint
  :ensure t
  :general
  (:keymaps 'mode-specific-map
            :prefix-map 'link-hint-keymap
            :prefix "o"
            "s" 'custom/open-url-current-buffer
            "f" 'link-hint-open-link
            "y" 'link-hint-copy-link
            "F" 'link-hint-open-multiple-links
            "Y" 'link-hint-copy-multiple-links)
  :custom
  (link-hint-avy-style 'de-bruijn))

(use-package ace-link
  :ensure t
  :after (link-hint) ; to use prefix keymap
  :general
  (:keymaps 'link-hint-keymap
            "l" 'counsel-ace-link)
  :config
  (ace-link-setup-default))

(use-package browse-url
  :if (and (eq system-type 'gnu/linux)
           (eq window-system 'x))
  :preface
  (defun custom/buffer-urls--candidates ()
    (save-excursion
      (save-restriction
        (let ((urls))
          (goto-char (point-min))
          (while (re-search-forward org-plain-link-re nil t)
            (push (thing-at-point 'url) urls))
          (remove nil urls)))))
  (defun custom/open-url-current-buffer ()
    (interactive)
    (ivy-read "URLs: "
              (funcall #'custom/buffer-urls--candidates)
              :action #'(lambda (candidate)
                          (browse-url candidate))
              :require-match t
              :caller 'custom/open-url-current-buffer))
  (defun feh-browse (url &rest ignore)
    "Browse image using feh."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process (concat "feh " url) nil "feh" url))
  (defun mpv-browse (url &rest ignore)
    "Browse video using mpv."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process (concat "mpv --loop-file=inf" url) nil "mpv" "--loop-file=inf" url))
  :secret "media.el.gpg"
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "xdg-open"))

(use-package beginend
  :ensure t
  :delight beginend-global-mode beginend-prog-mode beginend-magit-status-mode
  :config
  (beginend-global-mode))

(use-package mwim
  :ensure t
  :general
  ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] 'mwim-end-of-code-or-line))

(use-package bln-mode
  :ensure t
  :defer t
  :general
  (:keymaps 'mode-specific-map
            "h" 'bln-backward-half
            "j" 'bln-forward-half-v
            "k" 'bln-backward-half-v
            "l" 'bln-forward-half))

(use-package projectile
  :ensure t
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

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :preface
  (defun custom/open-project-todos ()
    (interactive)
    (let ((todos-file (expand-file-name "todo.org" (projectile-project-root))))
      (condition-case nil
          (when (file-exists-p todos-file)
            (find-file todos-file))
        (error (message "Cannot find project todos")))))
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
  (defun counsel-projectile-switch-project-action-open-todos (project)
    "Open project's TODOs."
    (let ((projectile-switch-project-action #'custom/open-project-todos))
      (counsel-projectile-switch-project-by-name project)))
  :general
  (:keymaps 'ctl-x-map
            "j j" 'counsel-projectile-switch-project
            "b" 'custom/ensure-project-switch-buffer)
  (:prefix "<f8>"
           "i" 'projectile-invalidate-cache
           "k" 'projectile-kill-buffers
           "c" 'projectile-commander
           "d" 'projectile-dired
           "f" 'projectile-recentf
           "t" 'custom/open-project-todos
           "T" 'doom/ivy-tasks
           "h" 'projectile-find-file)
  :config
  (counsel-projectile-mode 1)
  (add-to-list 'counsel-projectile-switch-project-action
               '("t" counsel-projectile-switch-project-action-open-todos "open project's todos") t)
  (setq projectile-switch-project-action 'counsel-projectile-switch-project))

(use-package dired
  :commands dired
  :hook (dired-mode-hook . auto-revert-mode)
  :general
  ([remap list-directory] 'dired)
  (:keymaps 'dired-mode-map
            "e" '(lambda ()
                   (interactive)
                   (when (derived-mode-p 'dired-mode)
                     (if (file-directory-p (dired-get-filename))
                         (message "Directories cannot be opened in EWW")
                       (eww-open-file (dired-get-file-for-visit)))))
            "C-x C-k" 'dired-do-delete)
  :preface
  (defvar custom/large-file-ok-types
    (rx "." (or "mp4" "mkv" "pdf") string-end)
    "Regexp matching filenames which are definitely ok to visit,
     even when the file is larger than `large-file-warning-threshold'.")
  (defadvice abort-if-file-too-large (around custom/check-large-file-ok-types)
    "If FILENAME matches `custom/large-file-ok-types', do not abort."
    (unless (string-match-p custom/large-file-ok-types (ad-get-arg 2))
      ad-do-it))
  (defadvice dired-do-rename (after revert-buffer activate)
    (revert-buffer))
  (defadvice dired-create-directory (after revert-buffer activate)
    (revert-buffer))
  :custom
  (dired-recursive-deletes 'top) ;; Allows recursive deletes
  (dired-dwim-target t)
  (dired-listing-switches "-lah1v --group-directories-first") ;;TODO: think of using TIME_STYLE env var
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (ad-activate 'abort-if-file-too-large)
  (use-package dired-filetype-face :ensure t)
  (use-package wdired
    :preface
    (defadvice wdired-abort-changes (after revert-buffer activate)
      (revert-buffer))
    :general
    (:keymaps 'dired-mode-map
              "r" 'wdired-change-to-wdired-mode)
    :custom
    (wdired-allow-to-change-permissions 'advanced))
  (use-package dired-launch
    :ensure t
    :hook
    (dired-mode . dired-launch-mode))
  (use-package dired-git-info
    :ensure t
    :bind
    (:map dired-mode-map
          (")" . dired-git-info-mode)))
  (use-package dired-narrow
    :ensure t
    :general
    (:keymaps 'dired-mode-map
              "/" 'dired-narrow))
  (use-package dired-quick-sort
    :ensure t
    :config
    (dired-quick-sort-setup))
  (use-package diredfl
    :ensure t
    :hook
    (dired-mode . diredfl-mode))
  (use-package dired-x
    :general
    ([remap list-directory] 'dired-jump)
    :custom
    ;; do not bind C-x C-j, it may be binded later
    (dired-bind-jump nil))
  (use-package dired-hide-dotfiles
    :ensure t
    :after (dired)
    :general
    (:keymaps 'dired-mode-map
              "." 'dired-hide-dotfiles-mode)
    :hook
    (dired-mode . dired-hide-dotfiles-mode)))

(use-package rg
  :ensure t
  :general
  (:keymaps 'mode-specific-map
            "r" 'rg
            "d" 'rg-project
            "m" 'rg-dwim)
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

(use-package phi-search
  :ensure t
  :commands phi-search phi-search-backward
  :hook (isearch-mode-hook . phi-search-from-isearch-mc/setup-keys)
  :config
  (use-package phi-search-mc
    :ensure t
    :config
    (phi-search-mc/setup-keys)))

(defadvice occur-mode-goto-occurrence (after close-occur activate)
  (delete-other-windows))

(use-package imenu-anywhere
  :ensure t
  :commands ivy-imenu-anywhere)

;; inline tasks navigation
(use-package doom-todo-ivy
  :quelpa
  (doom-todo-ivy :repo "jsmestad/doom-todo-ivy" :fetcher github)
  :commands doom/ivy-tasks)

(use-package ace-window
  :ensure t
  :after (avy)
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

(use-package swiper
  :ensure t
  :commands swiper swiper-multi swiper-occur
  :preface
  (defun custom/swiper (&optional tap)
    (interactive "P")
    (if tap
        (swiper-isearch (thing-at-point 'symbol))
      (swiper-isearch)))
  :general
  ("C-s" 'custom/swiper)
  :config
  (general-unbind 'global "C-s")
  :custom
  (swiper-include-line-number-in-search t)
  :custom-face (swiper-match-face-1 ((t (:background "#dddddd"))))
  :custom-face (swiper-match-face-2 ((t (:background "#bbbbbb" :weight bold))))
  :custom-face (swiper-match-face-3 ((t (:background "#bbbbff" :weight bold))))
  :custom-face (swiper-match-face-4 ((t (:background "#ffbbff" :weight bold)))))

(use-package avy
  :ensure t
  :general
  ("C-:" 'avy-goto-char)
  ("M-s M-s" 'avy-goto-word-0)
  :custom
  (avy-timeout-seconds 0.5)
  (avy-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :custom-face (avy-goto-char-timer-face ((nil (:foreground "green" :weight bold))))
  :config
  (avy-setup-default))

(use-package avy-zap
  :ensure t
  :general
  ([remap zap-to-char] 'avy-zap-to-char-dwim))
