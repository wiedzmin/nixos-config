;; TODO: setup xref package, then search for some modern completion UI (ivy/vertico/etc.) xref impl

(use-package navigation-misc
  @recenterWindowDisabled@
  :bind
  ("C-l" . custom/recenter-window))

(use-package avy
  :preface
  (defun avy-generic-command-action (action-f)
    "Executes action-f at point and stays"
    (save-excursion
      (goto-char pt)
      (funcall action-f))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char (+ 1 pt)))
  (defun avy-action-helpful (pt)
    (avy-generic-command-action #'helpful-at-point))
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-copy-tap (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'sexp)
        (copy-region-as-kill start end)
        (message "Copied: %s" (buffer-substring start end))))
    (let ((dat (ring-ref avy-ring 0)))
      (select-frame-set-input-focus
       (window-frame (cdr dat)))
      (select-window (cdr dat))
      (goto-char (car dat)))
    t)
  (defun avy-action-websearch (pt)
    "Search web for sexp at PT."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'sexp) ;TODO: fine-tune TAP type
        (let ((term (buffer-substring start end)))
          (message "searching for: %s" term)
          (call-process "@websearchBinary@" nil 0 nil "--term" term))))
    t)
  (defun avy-action-open-url (pt)
    "Open URL at PT."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'url)
        (let ((url (buffer-substring start end)))
          (message "opening %s" url)
          (browse-url url)
          )))
    t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-kill-whole-line-stay (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (let ((dat (ring-ref avy-ring 0)))
      (select-frame-set-input-focus
       (window-frame (cdr dat)))
      (select-window (cdr dat))
      (goto-char (car dat)))
    t)
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))
  (defun avy-action-clone-line (pt)
    (goto-char pt)
    (move-beginning-of-line 1)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end))
    (yank)
    (indent-for-tab-command)
    t)
  :bind
  ("C-:" . avy-goto-char)
  (:map custom-goto-map
        ("M-d" . avy-goto-word-0)
        ("M-s" . avy-goto-char-timer)
        ("," . pop-global-mark))
  (:map isearch-mode-map
        ("M-s" . avy-isearch))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-keys '(?q ?w ?e ?a ?s ?d ?z ?x ?c))
  (avy-linum-mode t)
  (avy-background t)
  (avy-all-windows 'all-frames)
  (avy-single-candidate-jump nil)
  (setq avy-style 'at-full)
  :custom-face
  (avy-goto-char-timer-face ((nil (:foreground "green" :weight bold))))
  (avy-lead-face ((nil (:foreground "black" :background "gold" :weight bold))))
  (avy-lead-face-0 ((nil (:foreground "white" :background "blue" :weight bold))))
  (avy-lead-face-1 ((nil (:foreground "white" :background "green" :weight bold))))
  (avy-lead-face-2 ((nil (:foreground "white" :background "red" :weight bold))))
  :config
  ;NOTE: removed 'avy-dispatch-alist vs 'avy-keys conflicts
  (use-package isearch)
  (setf avy-dispatch-alist (assq-delete-all ?z avy-dispatch-alist)
        avy-dispatch-alist (assq-delete-all ?x avy-dispatch-alist)
        avy-dispatch-alist (assq-delete-all ?c avy-dispatch-alist))
  (setf (alist-get ?Z avy-dispatch-alist) 'avy-action-zap-to-char
        (alist-get ?E avy-dispatch-alist) 'avy-action-embark
        (alist-get ?. avy-dispatch-alist) 'avy-action-mark-point
        (alist-get ?\C-w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?\M-w avy-dispatch-alist) 'avy-action-copy-tap
        (alist-get ?S avy-dispatch-alist) 'avy-action-websearch
        (alist-get ?u avy-dispatch-alist) 'avy-action-open-url
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line-stay
        (alist-get ?\M-k avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?X avy-dispatch-alist) 'avy-action-exchange
        (alist-get ?\C-h avy-dispatch-alist) 'avy-action-helpful
        (alist-get ?M avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?\C-l avy-dispatch-alist) 'avy-action-clone-line)
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
  (completion-styles '(orderless))
  (orderless-style-dispatchers '(dispatchers/without-if-bang
                                 dispatchers/flex
                                 dispatchers/initialism
                                 dispatchers/literal))
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((buffer (styles orderless-matching-styles partial-completion substring flex orderless-flex))
     (command (styles . (orderless)))
     (file (styles . (partial-completion initials orderless)))
     (function (styles . (orderless)))
     (info-menu (styles . (orderless)))
     (minibuffer (initials))
     (project-file (styles orderless) (cycle . t))
     (unicode-name (styles . (substring orderless)))
     (variable (styles . (orderless)))
     (xref-location (styles . (orderless)))))
  (orderless-component-separator "[ &-+]")
  :config
  (advice-add 'company-capf--candidates :around #'just-one-face))

(use-package embark
  :demand t
  :bind
  ;;TODO: embark-act-all
  ;;TODO: embark-bindings
  ("C-S-a" . embark-act)
  ("C-S-d" . embark-dwim)
  (:map embark-general-map
        ("C-." . embark-cycle))
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
  (:map iso-transl-ctl-x-8-map
        ("RET" . embark-save-unicode-character))
  ([remap kill-buffer] . embark-kill-buffer-and-window)
  :custom
  (embark-allow-edit-default t)
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator))
  (embark-prompter 'embark-completing-read-prompter)
  :config
  (add-to-list 'display-buffer-alist ;; Hide the mode line of the Embark live/completions buffers
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package marginalia
  :demand t
  :bind
  (:map minibuffer-local-map
        ("C-M-a" . marginalia-cycle))
  (:map minibuffer-local-completion-map
        ("C-M-a" . marginalia-cycle))
  (:map embark-general-map
        ("A" . marginalia-cycle))
  :config
  (marginalia-mode +1)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-align 'right))

(use-package ace-window
  :bind
  (:map custom-goto-map
        ("w" . ace-window))
  :custom
  (aw-background nil)
  (aw-dispatch-always t)
  (aw-leading-char-style 'char)
  (aw-make-frame-char ?r) ;NOTE: beware of further possible conflicts
  (aw-scope 'visible)
  (aw-keys '(?q ?w ?e ?a ?s ?d ?z ?x ?c))
  (aw-dispatch-alist '((?k aw-delete-window "Delete Window")
                       (?m aw-swap-window "Swap Windows")
                       (?M aw-move-window "Move Window")
                       (?C aw-copy-window "Copy Window")
                       (?j aw-switch-buffer-in-window "Select Buffer")
                       (?n aw-flip-window)
                       (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
                       (?f aw-split-window-fair "Split Fair Window")
                       (?v aw-split-window-vert "Split Vert Window")
                       (?h aw-split-window-horz "Split Horz Window")
                       (?o delete-other-windows "Delete Other Windows")
                       (?? aw-show-dispatch-help)))
  :config
  (ace-window-display-mode 1)
  :custom-face (aw-leading-char-face
                ((t (:inherit ace-jump-face-foreground
                              :foreground "green"
                              :height 2.0)))))

(use-package link-hint
  :bind
  (:map custom-goto-map
        ("M-f" . link-hint-open-link)
        ("M-F" . link-hint-open-multiple-links)
        ("M-y" . link-hint-copy-link)
        ("M-Y" . link-hint-copy-multiple-links))
  :custom
  (link-hint-avy-style 'at-full))

(use-package find-func
  :bind
  (:map custom-help-map
        ("l" . find-library)))

(use-package help
  :bind
  (:map help-map
        ("F" . describe-face))
  (:map custom-help-map
        ("F" . describe-face)
        ("b" . describe-bindings)
        ("i" . info-lookup-symbol)))

(use-package info-look
  :bind
  (:map custom-help-map
        ("i" . info-lookup-symbol)))

(use-package beginend
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
        ("n" . recursive-narrow-or-widen-dwim)
        ("W" . recursive-widen-dwim)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package dired
  :commands dired
  @autorevertEnable@
  :bind
  ([remap list-directory] . dired)
  (:map dired-mode-map
        ("/" . custom/dired-current-path-to-clipboard)
        ("i" . image-dired)
        ("C-c" . org-copy-visible))
  (:map dired-mode-map
        ("e" . (lambda ()
                 (interactive)
                 (when (derived-mode-p 'dired-mode)
                   (if (file-directory-p (dired-get-filename))
                       (message "Directories cannot be opened in EWW")
                     (eww-open-file (dired-get-file-for-visit)))))))
  :preface
  (defvar go-away-repl-regexp
    (rx bos "*" (or "Async")
        (zero-or-more nonl))
    "Regexp for matching windows to disappear")
  (defun custom/revert-dired-buffer (func &rest args) (revert-buffer))
  (defun custom/dired-current-path-to-clipboard ()
    (interactive)
    (when (eq major-mode 'dired-mode)
      (kill-new dired-directory)))
  :custom
  (dired-recursive-deletes 'top) ;; Allows recursive deletes
  (dired-dwim-target t)
  (dired-listing-switches "-lah1v --group-directories-first") ;;TODO: think of using TIME_STYLE env var
  (dired-recursive-copies 'always)
  (dired-filename-display-length 'window)
  (dired-vc-rename-file t)
  :config
  (use-package dired-x
    :custom
    (dired-guess-shell-alist-user '(("" "xdg-open"))))
  (when (version<= "28.0.50" emacs-version)
    (setq dired-kill-when-opening-new-dired-buffer nil))
  (put 'dired-find-alternate-file 'disabled nil)
  (add-to-list 'display-buffer-alist
               `(,go-away-repl-regexp
                 display-buffer-no-window
                 (inhibit-same-window . t)))
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
               (system-name))
           (buffer-name)
           (cond
            ((and (project-current) buffer-file-truename)
             (concat "(" (file-relative-name buffer-file-truename (cdr (project-current))) ")"))
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
  (:map custom-frame-map
        ("n" . make-frame-command)
        ("k" . mark-done-kill-frame)
        ("s" . delete-other-frames))
  (:map ctl-x-map
        ("C-c" . delete-frame)) ;; for keeping daemon running
  :config
  (add-hook 'pre-redisplay-functions 'keep-custom-frame-title)
  (blink-cursor-mode 0)
  (when (fboundp 'undelete-frame-mode)
    (undelete-frame-mode +1))
  (setq-default frame-title-format custom-frame-title-format) ;; for various external tools
  (setq truncate-partial-width-windows nil))

(use-package imenu-anywhere
  :commands imenu-anywhere
  :bind
  (:map custom-goto-map
               ("M-i" . imenu-anywhere)))

(use-package phi-search
  :hook (isearch-mode-hook . phi-search-from-isearch-mc/setup-keys)
  :config
  (use-package phi-search-mc
    :config
    (phi-search-mc/setup-keys)))

(use-package ripgrep
  :bind
  (:map custom-search-map
        ("f" . ripgrep-regexp)))

(use-package rg
  :bind
  (:map custom-search-map
        ("d" . rg-project)
        ("t" . rg-menu))
  :custom
  (rg-group-result nil)
  :config
  (rg-enable-default-bindings))

(use-package pulsar
  :bind
  (:map mode-specific-map
        ("h p" . pulsar-pulse-line)
        ("h h" . pulsar-highlight-line))
  :hook
  (imenu-after-jump-hook . pulsar-recenter-top)
  (imenu-after-jump-hook . pulsar-reveal-entry)
  (next-error-hook . pulsar-pulse-line)
  :custom
  (pulsar-pulse-functions '(recenter-top-bottom
                            move-to-window-line-top-bottom
                            reposition-window
                            forward-page
                            backward-page
                            scroll-up-command
                            scroll-down-command
                            org-next-visible-heading
                            org-previous-visible-heading
                            org-forward-heading-same-level
                            org-backward-heading-same-level
                            outline-backward-same-level
                            outline-forward-same-level
                            outline-next-visible-heading
                            outline-previous-visible-heading
                            outline-up-heading))
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 20)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1))

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

(use-package treemacs-icons-dired
  :after treemacs
  :config
  (treemacs-icons-dired-mode 1))

(use-package goggles
  :delight " 6d"
  :config
  (goggles-mode)
  (setq-default goggles-pulse t))

(use-package xref
  :preface
  (defun custom/revert-dired-buffer ()
    (delete-window (get-buffer-window (get-buffer "*xref*"))))
  :hook
  (xref--xref-buffer-mode-hook . hl-line-mode)
  ((xref-after-return-hook xref-after-jump-hook) . recenter)
  :bind
  ("M-?" . xref-find-references)
  ("M-[" . xref-pop-marker-stack)
  ("C-M-." . xref-find-apropos)
  (:map xref--xref-buffer-mode-map
        ("C-o" . xref-show-location-at-point)
        ("<tab>" . xref-quit-and-goto-xref)
        ("r" . xref-query-replace-in-results))
  :custom
  (xref-file-name-display 'project-relative)
  (xref-history-storage 'xref-window-local-history)
  (xref-marker-ring-length 1024)
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function #'xref-show-definitions-buffer)
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (advice-add 'xref-goto-xref :after #'custom/revert-dired-buffer))

(use-package dogears
  :demand t
  :config
  (dogears-mode +1))

(use-package bufler
  :bind
  ([remap list-buffers] . bufler-switch-buffer)
  :config
  (bufler-defgroups
  (group
   ;; Subgroup collecting all named workspaces.
   (auto-workspace))
  (group
   ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
   (group-or "*Help/Info*"
             (mode-match "*Help*" (rx bos "help-"))
             (mode-match "*Info*" (rx bos "info-"))))
  (group
   ;; Subgroup collecting all special buffers (i.e. ones that are not
   ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
   ;; through to other groups, so they end up grouped with their project buffers).
   (group-and "*Special*"
              (lambda (buffer)
                (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                     buffer)
                            (funcall (mode-match "Dired" (rx bos "dired"))
                                     buffer)
                            (funcall (auto-file) buffer))
                  "*Special*")))
   (group
    ;; Subgroup collecting these "special special" buffers
    ;; separately for convenience.
    (name-match "**Special**"
                (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
   (group
    ;; Subgroup collecting all other Magit buffers, grouped by directory.
    (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
    (auto-directory))
   ;; Subgroup for Helm buffers.
   (mode-match "*Helm*" (rx bos "helm-"))
   ;; Remaining special buffers are grouped automatically by mode.
   (auto-mode))
  ;; All buffers under "~/.emacs.d" (or wherever it is).
  (dir user-emacs-directory)
  (group
   ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
   ;; `org-directory' is not yet defined).
   (dir (if (bound-and-true-p org-directory)
            org-directory
          "~/org"))
   (group
    ;; Subgroup collecting indirect Org buffers, grouping them by file.
    ;; This is very useful when used with `org-tree-to-indirect-buffer'.
    (auto-indirect)
    (auto-file))
   ;; Group remaining buffers by whether they're file backed, then by mode.
   (group-not "*special*" (auto-file))
   (auto-mode))
  (group
   ;; Subgroup collecting buffers in a projectile project.
   (auto-projectile))
  (group
   ;; Subgroup collecting buffers in a version-control project,
   ;; grouping them by directory.
   (auto-project))
  ;; Group remaining buffers by directory, then major mode.
  (auto-directory)
  (auto-mode)))

(use-package burly
  ;;TODO: also lift frames bookmark opening up to `open-project' script
  ;;      level, otherwise be prepared for frames doubling in some cases
  :bind
  (:map custom-frame-map
        ("b" . burly-bookmark-windows)
        ("B" . burly-bookmark-frames))
  (:map custom-goto-map
        ("b" . burly-open-bookmark)
        ("M-b" . burly-open-last-bookmark)))

(use-package zygospore
  :bind
  ;FIXME: rebind
  ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package window
  :bind
  (:map mode-specific-map
        ("D" . toggle-window-dedicated))
  :custom
  (switch-to-buffer-obey-display-actions t))

(use-package epithet
  :load-path "@emacsEpithetPath@"
  :preface
  ;; FIXME: check if some interdepenencies enforcement is needed for rg being used here
  (defun epithet-for-rg ()
    "Suggest a name for an 'rg-mode' buffer"
    (when-let* (((derived-mode-p 'rg-mode))
                (command (car compilation-arguments))
                (searchterm (string-join
                             (cdr (member "-e" (split-string command " "))) " "))
                (filetype (cadr
                           (split-string
                            (car (seq-filter (lambda (l) (string-prefix-p "--type" l))
                                             (split-string command " "))) "="))))
      (format "\"%s\" <%s>"
              (replace-regexp-in-string (rx "\\" (group anything)) "\\1" searchterm)
              filetype)))
  :hook
  (Info-selection-hook . epithet-rename-buffer)
  (eww-after-render-hook . epithet-rename-buffer)
  (help-mode-hook . epithet-rename-buffer)
  (occur-mode-hook . epithet-rename-buffer)
  (shell-mode-hook . epithet-rename-buffer)
  (compilation-start-hook . epithet-rename-buffer-ignoring-arguments)
  (compilation-finish-functions . epithet-rename-buffer-ignoring-arguments)
  :config
  (add-to-list 'epithet-suggesters #'epithet-for-rg))

(use-package which-func
  :custom
  (which-function-mode 1))

(use-package isearch
  :custom
  (lazy-count-suffix-format "   (%s/%s)"))
