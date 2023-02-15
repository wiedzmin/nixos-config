;; TODO: review functionality / find substitute for / reimplement:
;; - doom-todo-ivy
;; - ivy-omni-org
;; - counsel-org-clock
;; TODO: setup xref package, then search for some modern completion UI (ivy/vertico/etc.) xref impl
;; TODO: avy-isearch

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
  (completion-styles '(basic partial-completion substring orderless)) ;TODO: try flex+emacs22 instead of orderless
  (orderless-style-dispatchers '(dispatchers/without-if-bang
                                 dispatchers/flex
                                 dispatchers/initialism
                                 dispatchers/literal))
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp))
  (completion-category-defaults nil)
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

;; TODO: consider trying `fd' finder tool, see https://github.com/minad/consult/wiki#find-files-using-fd for reference
;; Note: this requires lexical binding
(use-package consult
  :after dired
  :init
  (use-package consult-utils)
  :bind
  ("C-S-s" . consult-line-symbol-at-point)
  ("C-s" . consult-line)
  ("M-*" . consult-line-symbol-at-point)
  ("M-g" . consult-goto-line)
  ([remap apropos] . consult-apropos)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap man] . consult-man)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap yank-pop] . consult-yank-replace)
  (:map custom-search-map
        ("G" . consult-ripgrep-symbol-at-point)
        ("g" . consult-ripgrep)
        ("h" . consult-find)
        ("m" . consult-multi-occur))
  (:map help-map
        ("M" . consult-minor-mode-menu))
  (:map custom-help-map
        ("M" . consult-minor-mode-menu))
  (:map custom-projects-map
        ("O" . consult-file-externally))
  (:map custom-goto-map
        ("C-s" . consult-line-multi)
        ("i" . consult-imenu)
        ("I" . consult-imenu-multi)
        ("M-SPC" . consult-mark)
        ("`" . consult-compile-error)
        ("C" . consult-complex-command)
        ("j" . consult-global-mark)
        ("k" . consult-kmacro)
        ("o" . consult-outline)
        ("r b" . consult-bookmark)
        ("r l" . consult-register-load)
        ("r s" . consult-register-store)
        ("r x" . consult-register))
  (:map dired-mode-map
        ("`" . consult-file-externally))
  (:map minibuffer-local-map
        ("M-." . custom/embark-preview)
        ("<next>" . scroll-up-command)
        ("<prior>" . scroll-down-command))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult-async-input-debounce 0.1)
  (consult-async-input-throttle 0.2)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-line-point-placement 'match-beginning)
  (consult-line-start-from-top t)
  (consult-narrow-key "<")
  (register-preview-delay 0.1)
  (register-preview-function #'consult-register-format)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-recent-file :preview-key '("S-<up>" "S-<down>"))
  (advice-add #'register-preview :override #'consult-register-window)
  (consult-customize consult-line :keymap my-consult-line-map)
  (define-minibuffer-key "\C-s"
                         'consult-location #'previous-history-element
                         'file #'consult-find-for-minibuffer)
  (fset 'multi-occur #'consult-multi-occur))

(use-package consult-dir
  :bind
  ("C-x d" . consult-dir)
  :custom
  (consult-dir-project-list-function #'@consultDirProjectListFunction@)
  (consult-dir-default-command
   #'(lambda (&optional dirname switches)
       (interactive)
       (pop-to-buffer-same-window (dired-noselect dirname)))))

(use-package consult-xref
  :custom
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package bookmark-view
  :after consult
  :custom
  (consult-view-open-function #'bookmark-jump)
  (consult-view-list-function #'bookmark-view-names)
  :config
  (add-to-list 'consult-buffer-sources
               (list :name     "View"
                     :narrow   ?v
                     :category 'bookmark
                     :face     'font-lock-keyword-face
                     :history  'bookmark-view-history
                     :action   #'consult--bookmark-action
                     :items    #'bookmark-view-names)
               'append))

(use-package consult-flycheck
  :after (consult flycheck)
  :bind
  (:map mode-specific-map
        ("y" . consult-flycheck))
  (:map flycheck-mode-map
        ("C-c ! o" . consult-flycheck)
        ("!" . consult-flycheck)))

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
  :custom-face (aw-leading-char-face1
                ((t (:inherit ace-jump-face-foreground
                              :foreground "green"
                              :height 0.1)))))

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
        ("i" . image-dired))
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
  (dired-recursive-copies 'always)
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
            ((project-current)
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
  (unless (string-equal "i3" (getenv "CURRENT_WM"))
    (add-hook 'pre-redisplay-functions 'keep-custom-frame-title))
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
  :after (consult imenu)
  :bind
  (:map mode-specific-map
        ("h p" . pulsar-pulse-line)
        ("h h" . pulsar-highlight-line))
  :hook
  ((consult-after-jump-hook imenu-after-jump-hook) . pulsar-recenter-top)
  ((consult-after-jump-hook imenu-after-jump-hook) . pulsar-reveal-entry)
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
  :custom
  (xref-search-program 'ripgrep))

;;TODO: https://www.emacswiki.org/emacs/BookmarkPlus#AutomaticIdle-PeriodBookmarking
;;TODO: https://github.com/Overdr0ne/gumshoe
(use-package dogears
  :demand t
  :after consult
  :preface
  (defvar consult--source-dogears
    (list :name     "Dogears"
          :narrow   ?d
          :category 'dogears
          :items    (lambda ()
                      (mapcar
                       (lambda (place)
                         (propertize (dogears--format-record place)
                                     'consult--candidate place))
                       dogears-list))
          :action   (lambda (cand)
                      (dogears-go (get-text-property 0 'consult--candidate cand)))))
  (defun consult-dogears ()
    (interactive)
    (consult--multi '(consult--source-dogears)))
  :config
  (add-to-list 'consult-buffer-sources consult--source-dogears 'append)
  (dogears-mode +1))

;TODO: play with groups/workspaces (https://github.com/alphapapa/bufler.el#default-groups-example)
(use-package bufler
  :preface
  (defvar consult--bufler-workspace+
    (list :name "Workspace"
          :narrow ?w
          :category 'buffer
          :face 'consult-buffer
          :history 'buffer-name-history
          :state #'consult--buffer-state
          ;; :enabled (lambda () (frame-parameter nil 'bufler-workspace-path))
          :items (lambda ()
                   (let ((bufler-vc-state nil))
                     (mapcar #'buffer-name
                             (mapcar #'cdr
                                     (bufler-buffer-alist-at
                                      (frame-parameter nil 'bufler-workspace-path)
                                      :filter-fns bufler-filter-buffer-fns))))))
    "Bufler workspace buffers source for `consult-buffer'.")
  :bind
  ([remap list-buffers] . bufler-switch-buffer)
  :config
  (add-to-list 'consult-buffer-sources consult--bufler-workspace+ 'append))

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
  :custom
  (switch-to-buffer-obey-display-actions t))

(use-package consult-yasnippet
  :bind
  (:map misc-editing-map
        ("i" . consult-yasnippet)))
