(use-package doct
  :commands (doct))

;;TODO: relocate ob-* appropriately (add orgmode.enable options respectively)
(use-package ob-blockdiag
  :commands (org-babel-execute:blockdiag))

(use-package ob-css
  :commands (org-babel-execute:css
             org-babel-prep-session:css))

(use-package ob-dot
  :commands (org-babel-execute:dot
             org-babel-expand-body:dot))

(use-package ob-ditaa
  :commands (org-babel-execute:ditaa
             org-babel-prep-session:ditaa))

(use-package ob-emacs-lisp
  :commands (org-babel-execute:emacs-lisp
             org-babel-expand-body:emacs-lisp))

(use-package ob-lisp
  :commands (org-babel-execute:lisp
             org-babel-expand-body:lisp))

(use-package ob-js
  :commands (org-babel-execute:js
             org-babel-prep-session:js
             org-babel-variable-assignments:js))

(use-package ob-latex
  :commands (org-babel-execute:latex
             org-babel-expand-body:latex
             org-babel-prep-session:latex))

(use-package ob-org
  :commands (org-babel-execute:org
             org-babel-expand-body:org
             org-babel-prep-session:org))

(use-package ob-plantuml
  :commands (org-babel-execute:plantuml
             org-babel-prep-session:plantuml
             org-babel-variable-assignments:plantuml))

(use-package ob-scheme
  :commands (org-babel-execute:scheme
             org-babel-expand-body:scheme))

(use-package ob-python
  :commands (org-babel-execute:python))

(use-package ob-shell
  :commands (org-babel-execute:sh
             org-babel-expand-body:sh
             org-babel-execute:bash
             org-babel-expand-body:bash))

(use-package ob-async
  :after org ob)

(use-package ob-restclient
  :after ob restclient
  :commands (org-babel-execute:restclient))

(use-package org
  :after (f consult)
  :delight org-src-mode
  :preface
  ;; remove read-only props from yanked text (e.g. from jabber.el chat buffer)
  (defun custom/make-yank-writeable (func &rest args)
    (let ((inhibit-read-only t))
      (remove-text-properties (region-beginning) (region-end)
                              '(read-only t))))
  (defun custom/verify-refile-target () ;; Exclude DONE state tasks from refile targets
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  ;;TODO: customize "todo-only" parameter for "org-tags-view"
  (defun custom/follow-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
             With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view nil tag))              ;nil was (null current-prefix-arg) originally
  (defun custom/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "CLOCK" (point))))
  ;; (SEC-HIGH SEC-LOW MICROSEC PICOSEC) current-idle-time
  (defvar custom/idle-clockout-timeout 1800
    "Perform first attempt to clock-out after this period of emacs
              inactivity. It can decide to postpone the clocking-out if it's
          only emacs that is idle, but not the computer itself.")
  (defvar custom/idle-clockout-recheck-interval 300
    "After a sufficient idle time was achieved by emacs, we'll
              periodically check current idle time of the whole OS to decide
              whether we need to clock out")
  (defvar custom/idle-clockout-repeat-timer nil
    "Timer for repeatedly (during a single idle interval) checking
              whether we need to clock-out")
  (defun custom/clockout-when-idle ()
    (awhen custom/idle-clockout-repeat-timer
      (cancel-timer it))
    (when (org-clocking-p)
      (if (> (org-user-idle-seconds)
             custom/idle-clockout-timeout)
          (let ((org-clock-out-switch-to-state "WAITING")) ;TODO: introduce variable
            (org-clock-out nil t))
        (setf custom/idle-clockout-repeat-timer
              (run-with-idle-timer
               (time-add (current-idle-time) custom/idle-clockout-recheck-interval)
               nil
               'custom/clockout-when-idle)))))
  (defun custom/consult-ripgrep-org ()
    (interactive)
    (consult-ripgrep "@orgRoot@"))
  (defvar consult--source-org-buffer
    (list :name "Org"
          :narrow ?o
          :category 'buffer
          :state #'consult--buffer-state
          :items (lambda () (mapcar #'buffer-name (org-buffer-list)))))
  ;; https://github.com/chuntaro/emacs-keypression
  :mode (("\\.org$" . org-mode)
         ("\\.org_archive$" . org-mode))
  :company '(company-dabbrev company-capf)
  :capf #'pcomplete-completions-at-point
  :hook
  (org-mode-hook . turn-on-font-lock)
  (org-mode-hook . visual-line-mode)
  (org-clock-out-hook . custom/remove-empty-drawer-on-clock-out)
  (org-after-refile-insert-hook . save-buffer)
  (org-capture-mode-hook . (lambda ()
                             (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
  :bind
  (:map custom-org-map
        ("o" . ace-link-org)
        (";" . custom/org-tags-all)
        ("<down>" . org-forward-heading-same-level)
        ("<left>" . outline-previous-visible-heading)
        ("<right>" . outline-next-visible-heading)
        ("<up>" . org-backward-heading-same-level)
        ("=" . org-show-todo-tree)
        ("D" . org-delete-property)
        ("g" . custom/consult-ripgrep-org)
        ("G" . org-goto)
        ("S" . org-set-property)
        ("T" . org-table-create)
        ("i" . org-table-insert-row)
        ("," . org-table-move-row-up)
        ("." . org-table-move-row-down)
        ("a" . org-agenda)
        ("e" . org-capture)
        ("f" . ace-link-org)
        ("n" . org-narrow-to-subtree)
        ("R" . org-refile)
        ("s" . org-schedule)
        ("t" . org-toggle-timestamp-type)
        ("u" . outline-up-heading)
        ("v" . org-reveal)
        ("w" . org-store-link)
        ("x" . org-export-dispatch)
        ("y" . org-insert-link-global)
        ("|" . org-deadline)
        ("[" . org-agenda-file-to-front))
  (:map org-src-mode-map
        ("s-l" . org-edit-src-exit)
        ("C-c C-'" . org-edit-src-exit))
  (:map org-mode-map
        ("C-'" . nil)
        ("C-c [" . nil)
        ("C-c ]" . nil)
        ("C-c C-o" . nil)
        ("s-j" . org-babel-next-src-block)
        ("s-k" . org-babel-previous-src-block)
        ("s-l" . org-edit-src-code)
        ("C-c C-'" . org-edit-src-code))
  :custom-face (org-done ((nil (:foreground "PaleGreen" :weight normal :strike-through t))))
  :custom-face (org-headline-done ((nil (:foreground "LightSalmon" :weight normal :strike-through t))))
  :custom
  (agenda-opts-all-with-time
   '((org-agenda-todo-ignore-scheduled nil)
     (org-agenda-todo-ignore-deadlines nil)
     (org-agenda-todo-ignore-with-date nil)))
  (appt-display-interval 5)
  (appt-message-warning-time 10)
  (calendar-date-style 'european)
  (org-M-RET-may-split-line '((default . nil)))
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :narrow 60))
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-agenda-include-all-todo t)
  (org-agenda-include-diary t)
  (org-agenda-inhibit-startup t)
  (org-agenda-persistent-filter t)
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s %b")
                              (timeline . "  % s")
                              (todo . " %i %-12:c")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-repeating-timestamp-show-all nil)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-show-all-dates t)
  (org-agenda-show-future-repeats 'next)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-show-log t)
  (org-agenda-show-outline-path 'title)
  (org-agenda-skip-additional-timestamps-same-entry t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-span 'month)
  (org-agenda-start-on-weekday 1)
  (org-agenda-sticky nil) ;otherwise agenda behaves strangely on non-stuck projects
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-time-grid
   '((daily today require-timed remove-match)
     "----------------"
     (930 1000 1200 1400 1600 1800 2000 2200 2400 2500)))
  (org-agenda-todo-ignore-deadlines 'all)
  (org-agenda-todo-ignore-scheduled 'all)
  (org-agenda-todo-ignore-timestamp 'past)
  (org-agenda-todo-ignore-with-date t)
  (org-agenda-todo-list-sublevels nil)
  (org-agenda-use-tag-inheritance t)
  (org-agenda-window-setup 'current-window)
  (org-align-all-tags t)
  (org-archive-location "@orgRoot@/journals/journal.org::datetree/")
  (org-attach-directory "@orgRoot@/org-attach-data")
  (org-blank-before-new-entry '((heading) (plain-list-item . auto)))
  (org-catch-invisible-edits nil)
  ;;TODO: extend minimal clocking setup below
  (org-clock-history-length 35)
  (org-clock-idle-time 3) ;TODO: make variable and use here and in xidlehook
  (org-clock-in-resume t)
  (org-clock-in-switch-to-state "GOING")
  (org-clock-into-drawer "CLOCK")
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-out-switch-to-state "HOLD")
  (org-clock-persist t)
  ;; just clock-out unconditionally - it seems easier to maintain (credits to @binarin)
  (org-clock-x11idle-program-name "@xprintidleBinary@")
  (org-columns-default-format "%42ITEM %TODO %3Effort(E){:} %3CLOCKSUM_T(R) %SCHEDULED")
  (org-confirm-babel-evaluate nil)
  (org-confirm-elisp-link-function 'y-or-n-p)
  (org-confirm-elisp-link-not-regexp "org-tags-view")
  (org-confirm-shell-link-function 'y-or-n-p)
  (org-ctrl-k-protect-subtree t)
  (org-cycle-include-plain-lists 'integrate)
  (org-cycle-separator-lines 0)
  (org-deadline-warning-days 30)
  (org-default-notes-file "@orgRoot@/refile.org")
  (org-ditaa-jar-path "@ditaaJar@")
  (org-done-keywords-for-agenda '("DONE(d)" "CANCELLED(c)" "OUTDATED(o)"))
  (org-ellipsis (if (featurep 'unicode-fonts) "⤵" "…"))
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)  ;;TODO: try ORDERED/NOBLOCKING props : org-toggle-ordered-property
  (org-export-coding-system 'utf-8)
  (org-export-with-drawers t)
  (org-extend-today-until 2)
  (org-fast-tag-selection-include-todo nil)
  (org-fast-tag-selection-single-key 'expert)
  (org-fontify-done-headline t)
  (org-global-properties '(("STYLE_ALL" . "habit")))
  (org-goto-max-level 10)
  (org-hide-leading-stars t)
  (org-highlight-latex-and-related '(latex))
  (org-indirect-buffer-display 'current-window)
  (org-insert-mode-line-in-empty-file t)
  (org-log-done t)
  (org-log-into-drawer t)
  (org-log-repeat 'time)
  (org-log-state-notes-insert-after-drawers t)
  (org-loop-over-headlines-in-active-region t)
  (org-lowest-priority 70) ;; extend priorities set (given ascii code)
  (org-modules
   '(ol-bookmark ol-man org-checklist org-collector org-expiry org-id org-interactive-query org-protocol org-velocity))
  (org-outline-path-complete-in-steps nil)
  (org-priority-faces
   '((?A :foreground "red" :weight bold)
     (?B :foreground "#94bff3" :weight bold)
     (?C :foreground "#6f6f6f")
     (?D :foreground "#c390d4")
     (?E :foreground "#90c3d4")
     (?F :foreground "#a1d490")))
  (org-read-date-prefer-future 'time)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-target-verify-function 'custom/verify-refile-target)
  (org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))
  (org-refile-use-outline-path 'full-file-path)
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-speed-commands-user '(("x" org-todo "DONE")
                             ("y" org-todo-yesterday "DONE")
                             ("s" call-interactively 'org-schedule)
                             ("i" call-interactively 'org-clock-in)
                             ("o" call-interactively 'org-clock-out)
                             ("$" call-interactively 'org-archive-subtree)))
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-stuck-projects '("+LEVEL=1/-DONE" ("TODO" "GOING" "NEXT" "WAITING" "HOLD" "CANCELLED") nil ""))
  (org-tags-column -80)
  (org-tags-exclude-from-inheritance '("project"))
  (org-todo-keyword-faces
   '(("BACKLOG" . (:foreground "gray" :weight bold))
     ("SOON" . (:foreground "magenta" :weight bold))
     ("REPEAT" . (:foreground "blue" :weight bold))
     ("NEXT" . (:foreground "red" :weight bold))
     ("WAITING" . (:foreground "orange" :weight bold))
     ("FEEDBACK" . (:foreground "yellow" :weight bold))
     ("CANCELLED" . (:foreground "cyan" :weight bold))
     ("DONE" . (:foreground "green" :weight bold))))
  (org-todo-keywords '("BACKLOG(b)" "SOON(s)" "REPEAT(r)" "GOING(g!)" "NEXT(x)" "WAITING(w@/!)" "FEEDBACK"
                       "|" "DONE(d!/@)" "CANCELLED(c@/!)" "OUTDATED(o)"))
  (org-todo-keywords-for-agenda '("BACKLOG(b)" "SOON(s)" "REPEAT(r)" "GOING(g!)" "NEXT(x)" "WAITING(w@/!)" "FEEDBACK"))
  (org-todo-state-tags-triggers
   '(("GOING" ("current" . t))
     ("DONE" ("current"))))
  (org-track-ordered-property-with-tag t)
  (org-use-effective-time t)
  (org-use-property-inheritance t)
  (org-use-speed-commands t)
  (org-use-sub-superscripts nil)
  (org-x11idle-exists-p t)
  (org-yank-adjusted-subtrees t)
  :config
  (advice-add 'org-yank :after #'custom/make-yank-writeable)
  (require 'deferred)
  (deferred:$
    @pimOrgAgendaElPatch@
    (deferred:nextc
      (deferred:wait-idle 100)
      (lambda ()
        (push "/home/alex3rd/docs/org/bookmarks.org" org-agenda-files))))
  ;; run some commands
  (org-add-link-type "tag" 'custom/follow-tag-link)
  (org-clock-persistence-insinuate)
  (setq org-capture-templates ;TODO: elaborate capturing to roam
        (doct '(("feeds" :keys "f" :file "@orgRoot@/rss/feeds.org"
                 :children
                 (("blogs" :keys "b" :olp ("feeds" "blogs") :template "* @pimCommonCaptureDataTemplate@")
                  ("planets" :keys "p" :olp ("feeds" "planets") :template "* @pimCommonCaptureDataTemplate@")
                  ("LJ" :keys "l" :olp ("feeds" "LJ") :template "* @pimCommonCaptureDataTemplate@")
                  ("emacs" :keys "e" :olp ("feeds" "emacs") :template "* @pimCommonCaptureDataTemplate@")
                  ("rest" :keys "r" :olp ("feeds" "rest") :template "* @pimCommonCaptureDataTemplate@"))))))
  (run-with-idle-timer custom/idle-clockout-timeout t 'custom/clockout-when-idle)
  (turn-on-orgtbl)
  (add-to-list 'consult-buffer-sources consult--source-org-buffer 'append))

(use-package org-ql
  :after org
  :custom
  (org-agenda-custom-commands
   `(("dr" "Tasks to review"
      ((org-ql-block '(or (todo "BACKLOG")
                          (todo "SOON"))
                     ((org-ql-block-header "Tasks to review")))))
     ("dp" "Paused tasks"
      ((org-ql-block '(or (todo "WAITING")
                          (todo "FEEDBACK"))
                     ((org-ql-block-header "Paused tasks")))))
     ("ds" "Started tasks"
      ((org-ql-block '(or (todo "GOING")
                          (todo "FEEDBACK"))
                     ((org-ql-block-header "Started tasks")))))
     ("dc" "CANCELLED tasks" todo "CANCELLED" nil)
     ("ph" "High priority tasks"
      ((org-ql-block '(priority >= "B")
                     ((org-ql-block-header "High priority tasks")))))
     ("pl" "Low priority tasks"
      ((org-ql-block '(priority < "B")
                     ((org-ql-block-header "Low priority tasks")))))
     ("pn" "Unprioritized TODO entries"
      ((org-ql-block '(and (todo) (not (priority)))
                     ((org-ql-block-header "Unprioritized TODO entries")))))
     ("S" "Scheduled tasks"
      ((org-ql-block '(scheduled)
                     ((org-ql-block-header "Scheduled tasks"))))))))

(use-package org-bullets
  :hook
  (org-mode-hook . org-bullets-mode))

(use-package org-protocol
  :after org server)

(use-package org-rich-yank
  :after org
  :bind
  (:map org-mode-map
        ("C-M-y" . org-rich-yank)))

(use-package orgit
  :after org
  :bind
  (:map custom-org-map
        ("q" . orgit-store-link))
  :custom
  (orgit-store-reference t))

(use-package ox-html
  :commands (org-html-convert-region-to-html
             org-html-export-as-html
             org-html-export-to-html))

(use-package org-appear
  :hook
  (org-mode-hook . org-appear-mode)
  :custom
  (org-appear-autolinks t))

(use-package org-bars
  :if (display-graphic-p)
  :load-path "@emacsOrgBarsPath@"
  :hook
  (org-mode-hook . org-bars-mode))

;; TODO: review/debug graph generation under v2
;; TODO: review manual at https://www.orgroam.com/manual.html
;; TODO: review https://github.com/search?q=org-roam-capture-templates&type=code
;; TODO: review https://github.com/search?q=org-roam-capture-ref-templates&type=code
;; TODO: review https://www.reddit.com/r/OrgRoam/comments/or31g0/a_few_qol_tricks_i_havent_seen_much_of_on_the/
;; TODO: review https://melpa.org/#/org-roam-ui
;; TODO: review https://melpa.org/#/org-roam-timestamps / https://github.com/ThomasFKJorna/org-roam-timestamps
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "@emacsOrgRoamPath@")
  (org-roam-graph-executable "@emacsOrgRoamDotBinary@")
  (org-roam-db-location "@emacsOrgRoamPath@/org-roam.db")
  (org-roam-tag-sources '(prop vanilla all-directories))
  (org-roam-verbose t)
  (org-roam-mode-sections
        (list #'org-roam-backlinks-section :unique t
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\n") :unnarrowed t)))
  :config
  @orgRoamAutosyncEnable@
  (use-package org-roam-protocol)
  :bind
  (:map org-roam-map
        ("a" . org-roam-alias-add)
        ("A" . org-roam-alias-remove)
        ("t" . org-roam-tag-add)
        ("T" . org-roam-tag-remove)
        ("l" . org-roam-ref-add)
        ("L" . org-roam-ref-remove)
        ("f" . org-roam-node-find)
        ("F" . org-roam-ref-find)
        ("d" . org-roam-dailies-find-directory)
        ("i" . org-roam-node-insert)
        ("c" . org-roam-capture)
        ("r" . org-roam-refile)
        ("R" . org-roam-node-random)
        ("x" . org-roam-extract-subtree)
        ("." . org-roam-demote-entire-buffer)
        ("," . org-roam-promote-entire-buffer)
        ("j" . org-roam-dailies-capture-today)))

(use-package consult-org-roam
  :preface
  (defun avy-action-search-org-roam (pt)
    "Search org-roam corpus for sexp at PT."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'sexp) ;TODO: fine-tune TAP type
        (let ((term (buffer-substring start end)))
          (message "searching `org-roam' data for: %s" term)
          (consult-org-roam-search term))))
    t)
  (defun avy-action-search-url-org-roam (pt)
    "Search org-roam corpus for sexp at PT."
    (save-excursion
      (goto-char pt)
      (condition-case nil
          (cl-destructuring-bind (start . end)
              (bounds-of-thing-at-point 'url)
            (let ((term (buffer-substring start end)))
              (message "searching `org-roam' data for: %s" term)
              (consult-org-roam-search term)))
        (error
         (message "%s seems to not being an URL" (thing-at-point 'sexp)))))
    t)
  :delight " |>"
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  (consult-org-roam-mode 1)
  :bind
  (:map org-roam-map
        ("b" . consult-org-roam-backlinks)
        ("f" . consult-org-roam-file-find)
        ("g" . consult-org-roam-search))
  :config
  (setf (alist-get ?o avy-dispatch-alist) 'avy-action-search-org-roam
        (alist-get ?O avy-dispatch-alist) 'avy-action-search-url-org-roam))

(use-package org-roam-ui
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; TODO: https://github.com/nobiot/org-transclusion

(use-package orglink
  :delight " *>"
  :hook
  (emacs-lisp-mode-hook . orglink-mode)
  (nix-mode-hook . orglink-mode)
  (go-mode-hook . orglink-mode)
  :custom
  (orglink-match-anywhere t))

(use-package org-edit-indirect
  :hook
  (org-mode . org-edit-indirect-mode))

;;NOTE: example of inter-package dependencies
;;TODO: search and review other similar dependencies
(eval-after-load 'avy
  (use-package avy
    :bind
    (:map custom-org-map
          ("h" . avy-org-goto-heading-timer)
          ("^" . avy-org-refile-as-child))))

;; [[file:~/workspace/repos/github.com/wiedzmin/nixos-config/modules/pim/orgmode/todo.org::*https://github.com/akirak/akirak-mode/blob/1fa4845e5ad4af95b58c3cdba7ead7223f05cef0/akirak-consult-org.el#L7][TODO: review custom solutions]]
