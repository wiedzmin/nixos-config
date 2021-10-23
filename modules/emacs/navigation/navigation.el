;;TODO: review functionality / find substitute for / reimplement:
;; - doom-todo-ivy
;; - ivy-pass
;; - ivy-omni-org
;; - counsel-org-clock
;; TODO: https://github.com/minad/consult/issues/6
;; TODO: setup xref package, then search for selectrum/consult-bound xref impl
;; TODO: bind consult-error when compilation buffers will be used more extensively

(use-package avy
  :bind
  ("C-:" . avy-goto-char)
  (:map custom-goto-map
        ("M-s" . avy-goto-word-0))
  :custom
  (avy-timeout-seconds 0.5)
  (avy-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :custom-face (avy-goto-char-timer-face ((nil (:foreground "green" :weight bold))))
  :config
  (avy-setup-default))

(use-package goto-addr
 :hook ((compilation-mode-hook . goto-address-mode)
        (prog-mode-hook . goto-address-prog-mode))
 :bind (:map goto-address-highlight-keymap
             ("<RET>" . goto-address-at-point)
             ("M-<RET>" . newline))
 :commands (goto-address-prog-mode goto-address-mode))

(use-package manage-minor-mode-table
  :bind
  (:map mode-specific-map
        ("m" . manage-minor-mode-table)))

(use-package selectrum
  :demand t
  :init
  (use-package minibuffer-edit)
  :bind
  (:map selectrum-minibuffer-map
        ("C-'" . selectrum-quick-select)
        ("C-r" . selectrum-select-from-history)
        ("<S-backspace>" . minibuffer-edit-smart-delete-backwards)
        ("<C-S-backspace>" . selectrum-backward-kill-sexp)
        ("<escape>" . abort-minibuffers))
  (:map minibuffer-local-map
        ("M-h" . backward-kill-word))
  (:map mode-specific-map
        ("u" . selectrum-repeat)) ; "z"
  :custom
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (selectrum-count-style 'current/matches)
  (selectrum-files-select-input-dirs t)
  (selectrum-fix-vertical-window-height t)
  (selectrum-max-window-height .2) ; 20
  (selectrum-num-candidates-displayed 20)
  (selectrum-quick-keys '(?q ?w ?e ?a ?s ?d ?z ?x ?c))
  :custom-face
  (selectrum-current-candidate ((t (:background "#3a3f5a"))))
  :config
  (selectrum-mode +1))

(use-package prescient
  :commands prescient-persist-mode
  :custom
  (prescient-filter-method '(fuzzy initialism regexp literal))
  (prescient-history-length 1000)
  :config
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :after (selectrum prescient)
  :config
  ;; use Prescient on top of Orderless, see selectrum docs for details
  ;; https://github.com/raxod502/selectrum#user-content-alternative-2-orderless
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode +1))

(use-package orderless
  :preface
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  :init
  (use-package orderless-dispatchers)
  :bind
  (:map minibuffer-local-completion-map
        ("SPC" . nil)
        ;; SPC should never complete: use it for `orderless' groups.
        ("?" . nil))
  :custom
  ;;TODO: investigate if `completion-styles' setting is really _alternative_ against using `prescient'
  (completion-styles '(partial-completion substring orderless))
  (orderless-style-dispatchers '(dispatchers/selectrum-without-if-bang
                                 dispatchers/selectrum-with-if-equals
                                 dispatchers/flex
                                 dispatchers/initialism
                                 dispatchers/literal))
  (orderless-matching-styles '(orderless-regexp
                               orderless-initialism
                               orderless-flex))
  (completion-category-defaults nil)
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (completion-category-overrides
   '((buffer (orderless-matching-styles orderless-flex))
     (command (styles . (orderless)))
     (file (styles . (partial-completion initials orderless)))
     (function (styles . (orderless)))
     (info-menu (styles . (orderless)))
     (minibuffer (initials))
     (project-file (styles . (orderless)))
     (unicode-name (styles . (substring orderless)))
     (variable (styles . (orderless)))
     (xref-location (styles . (orderless)))))
  (orderless-component-separator "[ &]") ; TODO: try "[ -]"
  :config
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)) ;perf tip: highlight only visible candidates
  (advice-add 'company-capf--candidates :around #'just-one-face))

(use-package embark
  :demand t
  :preface
  (defun current-candidate+category ()
    (when selectrum-is-active
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidate))))
  (defun current-candidates+category ()
    (when selectrum-is-active
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidates
             ;; Pass relative file names for dired.
             minibuffer-completing-file-name))))
  :bind
  ("C-S-a" . embark-act)
  ("C-S-d" . embark-dwim)
  (:map mode-specific-map
        ("C-." . embark-act)
        ("C-," . embark-dwim))
  (:map help-map
        ("B" . embark-bindings))
  (:map minibuffer-local-map
        ("C-c x" . embark-export)
        (">" . embark-become)
        ("M-q" . embark-collect-toggle-view))
  (:map minibuffer-local-completion-map
        ("C-:" . embark-act))
  (:map completion-list-mode-map
        (";" . embark-act))
  (:map selectrum-minibuffer-map
        ("C-c x" . embark-export)
        ("M-c" . embark-collect-snapshot))
  (:map iso-transl-ctl-x-8-map
        ("RET" . embark-save-unicode-character))
  ([remap kill-buffer] . embark-kill-buffer-and-window)
  :hook
  (embark-setup-hook selectrum-set-selected-candidate)
  (embark-target-finders . current-candidate+category)
  (embark-candidate-collectors . current-candidates+category)
  (embark-pre-action-hook . (lambda () (setq selectrum--previous-input-string nil)))
  :custom
  (embark-allow-edit-default t)
  (embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
  (embark-prompter 'embark-completing-read-prompter)
  (embark-action-indicator (lambda (map) ;; integration with which-key
                             (which-key--show-keymap "Embark" map nil nil 'no-paging)
                             #'which-key--hide-popup-ignore-command)
                           embark-become-indicator embark-action-indicator)
  :config
  (add-to-list 'display-buffer-alist ;; Hide the mode line of the Embark live/completions buffers
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'embark-allow-edit-actions 'consult-imenu))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :bind
  (:map embark-file-map
        ("x" . consult-file-externally)
        ("j" . find-file-other-window))
  (:map embark-buffer-map
        ("j" . consult-buffer-other-window))
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package marginalia
  :demand t
  :preface
  (defun custom/annotate-project-buffer-file (buffer)
    "Return the file or process name of BUFFER relative to project root, if it is within project root."
    (let ((root (marginalia--project-root))
          (file (marginalia--buffer-file buffer)))
        (if (string-equal root file) file
          (string-remove-prefix root file))))
  (defun custom/annotate-project-buffer (cand)
    "Annotate project buffer CAND with modification status, file name and major mode."
    (when-let (buffer (get-buffer cand))
      (marginalia--fields
       ((marginalia--buffer-status buffer))
       ((custom/annotate-project-buffer-file buffer)
        :truncate (/ marginalia-truncate-width 2)
        :face 'marginalia-file-name))))
  :bind
  (:map minibuffer-local-map
        ("C-M-a" . marginalia-cycle))
  (:map minibuffer-local-completion-map
        ("C-M-a" . marginalia-cycle))
  (:map embark-general-map
        ("A" . marginalia-cycle))
  :config
  ;; Default command category to 'marginalia-annotate-binding instead of
  ;; 'marginalia-annotate-command which has a slight performance impact when
  ;; filtering M-x candidates.
  (mapc
   (lambda (x)
     (pcase (car x) ('command (setcdr x (cons 'marginalia-annotate-binding
                                              (remq 'marginalia-annotate-binding (cdr x)))))))
   marginalia-annotator-registry)
  (add-to-list 'marginalia-annotator-registry '(project-buffer custom/annotate-project-buffer))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit 'keep-selected))))
  (marginalia-mode +1)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package consult
  :bind
  ("C-s" . consult-line)
  ("C-S-s" . consult-line-symbol-at-point)
  ("M-<f12>" . consult-buffer)
  ("M-y" . consult-yank-replace)
  ("M-g" . consult-goto-line)
  (:map custom-nav-map
        ("g" . consult-ripgrep)
        ("i" . consult-imenu) ; consult-project-imenu
        ("k" . consult-kmacro)
        ("x" . consult-register)
        ("j" . consult-global-mark)
        ("o" . consult-outline)
        ("c" . consult-complex-command)
        ("r l" . consult-register)
        ("r s" . consult-register-store)
        ("m" . consult-multi-occur))
  (:map dired-mode-map
        ("`" . consult-file-externally))
  (:map ctl-x-map
        ("b" . consult-buffer)
        ("C-b" . consult-bookmark)
        ("C-r" . consult-recentf-file)
        ("m" . consult-minor-mode-menu))
  (:map help-map
        ("a" . consult-apropos)
        ("C-m" . consult-man))
  :custom
  (register-preview-delay 0)
  (register-preview-function #'consult-register-preview)
  (consult-project-root-function #'projectile-project-root)
  (consult-line-point-placement 'match-end)
  :config
  (fset 'multi-occur #'consult-multi-occur)
  (fset 'projectile-ripgrep 'consult-ripgrep))

(use-package bookmark-view
  :load-path "@emacsBookmarkViewPath@"
  :custom
  (consult-view-open-function #'bookmark-jump)
  (consult-view-list-function #'bookmark-view-names))

(use-package consult-projectile
  :load-path "@emacsConsultProjectilePath@"
  :after projectile
  :bind
  ("C-<f1>" . consult-projectile))

(use-package consult-flycheck
  :after consult
  :bind
  (:map mode-specific-map
        ("y" . consult-flycheck))
  (:map flycheck-mode-map
        ("C-c ! o" . consult-flycheck)))

(use-package ace-window
  :bind
  ("M-RET" . ace-window)
  :custom
  (aw-background nil)
  (aw-leading-char-style 'char)
  (aw-scope 'visible)
  :config
  (ace-window-display-mode 1)
  :custom-face (aw-leading-char-face1
                ((t (:inherit ace-jump-face-foreground
                              :foreground "green"
                              :height 0.1)))))

(use-package link-hint
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

(use-package find-func
  :bind
  (:map custom-help-map
        ("l" . find-library)))

(use-package help
  :bind
  (:map custom-help-map
        ("b" . describe-bindings)
        ("i" . info-lookup-symbol)))

(use-package info-look
  :bind
  (:map custom-help-map
        ("i" . info-lookup-symbol)))

(use-package beginend
  :if (string-equal "i3" (getenv "CURRENT_WM"))
  :config
  (delight '((beginend-global-mode nil "beginend")
             (beginend-prog-mode nil "beginend")
             (beginend-dired-mode nil "beginend")
             (beginend-compilation-mode nil "beginend")
             (beginend-rg-mode nil "beginend")
             (beginend-magit-status-mode nil "beginend")
             (beginend-org-mode nil "beginend")
             (beginend-outline-mode nil "beginend")))
  (beginend-global-mode))

(use-package flycheck-projectile
  :after (projectile flycheck)
  :bind
  (:map mode-specific-map
        ("p" . flycheck-projectile-list-errors))
  (:map flycheck-mode-map
        ("C-c ! p" . flycheck-projectile-list-errors)))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] . mwim-end-of-code-or-line))

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package recursive-narrow
  :bind
  (:map custom-narrowing-map
        ("r" . narrow-to-region)
        ("d" . narrow-to-defun)
        ("w" . widen)
        ("N" . recursive-narrow-or-widen-dwim)
        ("D" . recursive-widen-dwim)))

(use-package whitespace
  :hook
  ((prog-mode-hook text-mode-hook) . whitespace-turn-on)
  (org-mode-hook . whitespace-turn-off)
  :bind
  (:map custom-ws-map
        ("w" . whitespace-mode))
  :custom
  (whitespace-line-column 121)
  (whitespace-style '(indentation::space
                      space-after-tab
                      space-before-tab
                      trailing
                      lines-tail
                      tab-mark
                      face
                      tabs)))

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
  (use-package dired-x
    :custom
    (dired-guess-shell-alist-user '(("" "xdg-open"))))
  (when (version<= "28.0.50" emacs-version)
    (setq dired-kill-when-opening-new-dired-buffer nil))
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
  :commands imenu-anywhere
  :bind
  (:map custom-nav-map
               ("I" . imenu-anywhere)))

(use-package phi-search
  :hook (isearch-mode-hook . phi-search-from-isearch-mc/setup-keys)
  :config
  (use-package phi-search-mc
    :config
    (phi-search-mc/setup-keys)))

(use-package projectile
  :after selectrum
  :delight
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
  (defun projectile-switch-project-action-codesearch-search (project)
    "Search project's files with Codesearch."
    (let ((projectile-switch-project-action #'projectile-codesearch-search))
      (projectile-switch-project-by-name project)))
  (defun projectile-switch-project-action-open-todos (project)
    "Open project's TODOs."
    (let ((projectile-switch-project-action #'custom/open-project-todos))
      (projectile-switch-project-by-name project)))
  (defun projectile-switch-project-action-open-magit-status (project)
    "Open project's Magit status buffer."
    (let ((projectile-switch-project-action #'custom/open-project-magit-status))
      (projectile-switch-project-by-name project)))
  :bind
  (:map custom-projectile-map
        ("C" . projectile-commander)
        ("d" . projectile-dired)
        ("i" . projectile-invalidate-cache)
        ("k" . projectile-kill-buffers)
        ("t" . custom/open-project-todos)
        ("m" . custom/open-project-magit-status)
        ("f" . recentf-open-files)
        ("h" . projectile-find-file))
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-require-project-root t)
  (projectile-track-known-projects-automatically nil)
  (projectile-project-search-path
   '("~/workspace/repos"
     "~/workspace/repos.stale"))
  (projectile-project-root-functions
   '(
     projectile-root-local
     projectile-root-bottom-up
     ;; projectile-root-top-down
     ;; projectile-root-top-down-recurring
     ))
  (projectile-project-root-files '(@projectsRootMarkersEmacs@))
  :hook
  (after-init-hook . projectile-mode)
  :config
  :delight '(:eval (concat " ¶[" (projectile-project-name) "]")))

(use-package rg
  :bind
  (:map custom-nav-map
        ("f" . rg)
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
  (:map custom-treemacs-map
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

(use-package magit-todos
   :after (magit projectile)
   :bind
   (:map mode-specific-map
         ("C-d" . magit-todos-list))
   (:map custom-projectile-map
         ("T" . magit-todos-list)))

(use-package xref
  :custom
  (xref-search-program 'ripgrep))

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
