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
  :init
  (use-package orderless-dispatchers)
  :bind
  (:map minibuffer-local-completion-map
        ("SPC" . nil)
        ;; SPC should never complete: use it for `orderless' groups.
        ("?" . nil))
  :custom
  ;;TODO: investigate if `completion-styles' setting is really _alternative_ against using `prescient'
  (completion-styles '(orderless partial-completion basic))
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
  (use-package orderless-kwd
    :config
    (add-to-list 'orderless-style-dispatchers #'orderless-kwd-dispatch))
  (define-advice company-capf--candidates
      (:around (orig-fun &rest args) just-one-face)
    (let ((orderless-match-faces [completions-common-part]))
      (apply orig-fun args))))

(use-package embark
  :demand t
  :preface
  (defun custom/embark-toggle-prompter ()
    (interactive)
    (if (eq embark-prompter 'embark-completing-read-prompter)
        (setq embark-prompter 'embark-keymap-prompter)
      (setq embark-prompter 'embark-completing-read-prompter)))
  :bind
  ("C-S-a" . embark-act)
  ("C-*" . embark-act-all)
  ("C-S-d" . embark-dwim)
  ("C->" . embark-select)
  ("C-&" . custom/embark-toggle-prompter)
  (:map embark-general-map
        ("C-." . embark-cycle))
  (:map mode-specific-map
        ("C-." . embark-act)
        ("C-," . embark-dwim))
  (:map help-map
        ("B" . embark-bindings))
  (:map minibuffer-local-map
        ("C-c x" . embark-export)
        ("C-c b" . embark-become))
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
  (prefix-help-command 'embark-prefix-help-command))

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
  (marginalia-align 'left))

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
  (aw-dispatch-alist '((?\% aw-swap-window "Swap Windows")
                       (?\> aw-move-window "Move Window")
                       (?\@ aw-copy-window "Copy Window")
                       (?f aw-flip-window)
                       (?b aw-switch-buffer-in-window "Select Buffer")
                       (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
                       (?\= aw-split-window-fair "Split Fair Window")
                       (?\- aw-split-window-vert "Split Vert Window")
                       (?\| aw-split-window-horz "Split Horz Window")
                       (?k aw-delete-window "Delete Window")
                       (?K delete-other-windows "Delete Other Windows")
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
        ("M-y" . link-hint-copy-link))
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
        ("b" . describe-bindings)))

(use-package shortdoc
  :bind
  (:map help-map
        ("/" . shortdoc-display-group))
  (:map custom-help-map
        ("/" . shortdoc-display-group)))

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

(use-package mosey
  :bind
  ([remap move-beginning-of-line] . mosey-custom-backward-bounce)
  ([remap move-end-of-line] . mosey-custom-forward-bounce)
  :config
  (defmosey '(beginning-of-line
              back-to-indentation
              mosey-goto-end-of-code
              mosey-goto-beginning-of-comment-text
              end-of-line)
  :prefix "custom"))

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
  :preface
  (defvar go-away-repl-regexp
    (rx bos "*" (or "Async")
        (zero-or-more nonl))
    "Regexp for matching windows to disappear")
  :commands dired
  @autorevertEnable@
  :bind
  ([remap list-directory] . dired)
  (:map dired-mode-map
        ("i" . image-dired)
        ("C-c" . org-copy-visible))
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
  (use-package dired-filetype-face)
  (when (version<= "28.0.50" emacs-version)
    (setq dired-kill-when-opening-new-dired-buffer nil))
  (put 'dired-find-alternate-file 'disabled nil)
  (add-to-list 'display-buffer-alist
               `(,go-away-repl-regexp
                 display-buffer-no-window
                 (inhibit-same-window . t)))
  (dolist (func '(dired-do-rename dired-create-directory))
    (define-advice func
        (:after (orig-fun &rest args) custom/revert-dired-buffer)
      (revert-buffer))))

(use-package dired-aux
  :bind
  (:map misc-editing-map
        ("d" . dired-do-replace-regexp-as-diff)))

(use-package wdired
  :after dired
  :custom
  (wdired-allow-to-change-permissions 'advanced)
  :config
  (define-advice wdired-abort-changesfunc
      (:after (orig-fun &rest args) custom/revert-dired-buffer)
    (revert-buffer)))

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

(use-package rg
  :bind
  (:map custom-search-map
        ("d" . rg-regexp-current-dir)
        ("p" . rg-project)
        ("m" . rg-menu))
  :custom
  (rg-group-result nil)
  :config
  (rg-define-search rg-regexp-current-dir
    "Search for regexp in files under the current directory."
    :query ask
    :format regexp
    :files ask
    :dir current)
  (rg-enable-default-bindings)
  (define-advice rg-run
      (:before (orig-fun &rest args) custom/rg-run-before)
    (rg-save-search)))

(use-package pulsar
  :bind
  (:map mode-specific-map
        ("h p" . pulsar-pulse-line)
        ("h r" . pulsar-pulse-region)
        ("h l" . pulsar-highlight-line))
  :hook
  (next-error-hook . pulsar-pulse-line)
  :custom
  (pulsar-pulse-functions '(backward-page
                            forward-page
                            goto-line
                            handle-switch-frame
                            move-to-window-line-top-bottom
                            narrow-to-defun
                            narrow-to-page
                            narrow-to-region
                            org-backward-heading-same-level
                            org-forward-heading-same-level
                            org-next-visible-heading
                            org-previous-visible-heading
                            outline-backward-same-level
                            outline-forward-same-level
                            outline-next-visible-heading
                            outline-previous-visible-heading
                            outline-up-heading
                            recenter-top-bottom
                            reposition-window
                            scroll-down-command
                            scroll-up-command
                            widen))
  (pulsar-pulse-region-functions '(delete-region
                                   kill-region
                                   primitive-undo
                                   transpose-chars
                                   transpose-lines
                                   transpose-words
                                   yank
                                   yank-pop))
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 20)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1))

(use-package xref
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
  (define-advice xref-goto-xref
      (:after (orig-fun &rest args) custom/revert-dired-buffer)
    (delete-window (get-buffer-window (get-buffer "*xref*")))))

(use-package bufler
  :bind
  ([remap list-buffers] . bufler-switch-buffer)
  ("<f12>" . bufler-list)
  :config
  (bufler-defgroups
    (group (auto-workspace))
    (group (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*Info*" (rx bos "info-"))))
    (group
     (group-and "*Special*"
                (lambda (buffer)
                  (unless (or (funcall (mode-match "Magit" (rx bos "magit-status")) buffer)
                              (funcall (mode-match "Dired" (rx bos "dired")) buffer)
                              (funcall (auto-file) buffer))
                    "*Special*")))
     (group (name-match "**Special**" (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
     (group (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-")) (auto-directory))
     (auto-mode))
    (dir user-emacs-directory)
    (group
     (dir "@orgRoamRootDir@")
     (group (auto-indirect) (auto-file))
     (group-not "*special*" (auto-file))
     (auto-mode))
    (group (auto-projectile))
    (group (auto-project))
    (auto-directory)
    (auto-mode)))

(use-package window
  :bind
  ("C-l" . recenter-top-bottom)
  (:map mode-specific-map
        ("D" . toggle-window-dedicated))
  :custom
  (switch-to-buffer-obey-display-actions t))

(use-package epithet
  :load-path "@emacsEpithetPath@"
  :preface
  (defun epithet-for-rg ()
    "Suggest a name for an 'rg-mode' buffer"
    (when (derived-mode-p 'rg-mode)
      (let* ((command (car compilation-arguments))
             (searchterm (string-join (cdr (member "-e" (split-string command " "))) " ")))
        (let ((filetype-data (seq-filter
                              (lambda (l) (string-prefix-p "--type" l))
                              (split-string command " "))))
          (format "rg <%s> [%s] at %s"
                  (replace-regexp-in-string (rx "\\" (group anything)) "\\1" searchterm)
                  (if filetype-data (cadr (split-string (car filetype-data) "=")) "all")
                  compilation-directory)))))
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
  :bind
  ("C-M-s" . isearch-forward-regexp)
  ("C-M-r" . isearch-backward-regexp)
  ("C-M-;" . isearch-forward-thing-at-point)
  ("C-M-'" . isearch-backward-thing-at-point)
  :custom
  (lazy-count-suffix-format "   (%s/%s)"))

(use-package project-headerline
  :load-path "@emacsProjectHeaderlinePath@"
  :config
  (global-project-headerline-mode))

(use-package navigation-misc
  :bind
  (:map dired-mode-map
        ("/" . custom/dired-current-path-to-clipboard))
  (:map dired-mode-map
        ("e" . custom/dired-open-in-eww))
  (:map custom-projects-map
        ("b" . custom/kill-vc-current-buffer-file-path)))
