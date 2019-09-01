(use-package blockdiag-mode :ensure t)

(use-package calendar
  :custom
  (calendar-week-start-day 1))

(use-package ob-blockdiag
  :ensure t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((blockdiag . t))))

(use-package ob-css
  :ensure org-plus-contrib
  :commands (org-babel-execute:css
             org-babel-prep-session:css))

(use-package ob-dot
  :ensure org-plus-contrib
  :commands (org-babel-execute:dot
             org-babel-expand-body:dot))

(use-package ob-ditaa
  :ensure org-plus-contrib
  :commands (org-babel-execute:ditaa
             org-babel-prep-session:ditaa))

(use-package ob-emacs-lisp
  :ensure org-plus-contrib
  :commands (org-babel-execute:emacs-lisp
             org-babel-expand-body:emacs-lisp))

(use-package ob-lisp
  :ensure org-plus-contrib
  :commands (org-babel-execute:lisp
             org-babel-expand-body:lisp))

(use-package ob-js
  :ensure org-plus-contrib
  :commands (org-babel-execute:js
             org-babel-prep-session:js
             org-babel-variable-assignments:js))

(use-package ob-latex
  :ensure org-plus-contrib
  :commands (org-babel-execute:latex
             org-babel-expand-body:latex
             org-babel-prep-session:latex))

(use-package ob-org
  :ensure org-plus-contrib
  :commands (org-babel-execute:org
             org-babel-expand-body:org
             org-babel-prep-session:org))

(use-package ob-plantuml
  :ensure org-plus-contrib
  :commands (org-babel-execute:plantuml
             org-babel-prep-session:plantuml
             org-babel-variable-assignments:plantuml))

(use-package ob-scheme
  :ensure org-plus-contrib
  :commands (org-babel-execute:scheme
             org-babel-expand-body:scheme))

(use-package ob-python
  :ensure org-plus-contrib
  :commands (org-babel-execute:python))

(use-package ob-shell
  :ensure org-plus-contrib
  :commands (org-babel-execute:sh
             org-babel-expand-body:sh
             org-babel-execute:bash
             org-babel-expand-body:bash))

(use-package ob-async
  :ensure t
  :defer t
  :after org ob)

(use-package ob-restclient
  :ensure t
  :after ob restclient
  :commands (org-babel-execute:restclient))

(use-package org
  :ensure org-plus-contrib
  :after f
  :preface
  ;; remove read-only props from yanked text (e.g. from jabber.el chat buffer)
  (defadvice org-yank (after make-yank-writeable disable)
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
  :mode (("\\.org$" . org-mode)
         ("\\.org_archive$" . org-mode))
  :hook
  (org-mode-hook . turn-on-font-lock)
  (org-mode-hook . visual-line-mode)
  (org-clock-out-hook . custom/remove-empty-drawer-on-clock-out)
  (org-after-refile-insert-hook . save-buffer)
  (org-capture-mode-hook . (lambda ()
                             (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
  :bind
  (:map org-agenda-mode-map
        ("<f7> o" . ace-link-org))
  (:prefix-map custom-org-map
               :prefix "<f7>"
               (";" . custom/org-tags-all)
               ("<down>" . org-forward-heading-same-level)
               ("<left>" . outline-previous-visible-heading)
               ("<right>" . outline-next-visible-heading)
               ("<up>" . org-backward-heading-same-level)
               ("=" . org-show-todo-tree)
               ("D" . org-delete-property)
               ("G" . org-goto)
               ("H" . org-recent-headings-ivy)
               ("S" . org-set-property)
               ("T" . org-table-create)
               ("\\" . counsel-org-tag)
               ("a" . org-agenda)
               ("e" . org-capture)
               ("f" . ace-link-org)
               ("l" . counsel-org-agenda-headlines)
               ("n" . org-narrow-to-subtree)
               ("r" . org-refile)
               ("s" . org-schedule)
               ("t" . org-toggle-timestamp-type)
               ("u" . outline-up-heading)
               ("v" . org-reveal)
               ("w" . org-store-link)
               ("y" . org-insert-link-global)
               ("|" . org-deadline))
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
  (org-agenda-custom-commands
   `(("d" . "some non-straightforward TODO statuses")
     ("db" todo "BACKLOG" nil)
     ("ds" todo "SOON" nil)
     ("dc" todo "CANCELLED" nil)
     ("dw" todo "WAITING|FEEDBACK" nil)
     ("dg" todo "GOING" ,agenda-opts-all-with-time)
     ("P" . "Prioritized tasks")
     ("Pa" "Prioritized tasks A"
      ((tags-todo "+PRIORITY=\"A\"") ))
     ("Pb" "Prioritized tasks B"
      ((tags-todo "+PRIORITY=\"B\"")))
     ("Pc" "Prioritized tasks C"
      ((tags-todo "+PRIORITY=\"C\"")))
     ("Pd" "Prioritized tasks D"
      ((tags-todo "+PRIORITY=\"D\"") ))
     ("Pe" "Prioritized tasks E"
      ((tags-todo "+PRIORITY=\"E\"")))
     ("Pf" "Prioritized tasks F"
      ((tags-todo "+PRIORITY=\"F\"")))
     ("S" "Scheduled tasks" agenda ""
      ((org-agenda-time-grid nil)
       (org-deadline-warning-days 32)
       (org-agenda-entry-types '(:scheduled))))
     ("u" . "unassigned")
     ("up" alltodo "Unprioritized TODO entries"
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'regexp "\\[#[ABCDEF]]")))
       (org-tags-match-list-sublevels 'indented)
       (org-agenda-sorting-strategy
        '((agenda time-up tag-up) ))
       ;; '(org-agenda-sorting-strategy '((agenda time-up priority-down tag-up) (todo tag-up)))
       (org-agenda-overriding-header "Unprioritized TODO entries: ")))
     ("jc" tags "+@job+current/GOING|FEEDBACK")
     ))
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
  (org-archive-location "@emacsOrgJournalFile@::datetree/")
  (org-attach-directory "@emacsOrgAttachDir@")
  (org-blank-before-new-entry '((heading) (plain-list-item . auto)))
  (org-catch-invisible-edits nil)
  ;TODO: extend minimal clocking setup below
  (org-clock-history-length 35)
  (org-clock-idle-time 3) ;TODO: make variable and use here and in xidlehook
  (org-clock-in-resume t)
  (org-clock-in-switch-to-state "GOING")
  (org-clock-into-drawer "CLOCK")
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-out-switch-to-state "HOLD")
  (org-clock-persist t)
  ;; just clock-out unconditionally - it seems easier to maintain (credits to @binarin)
  (org-clock-x11idle-program-name "@emacsXprintidleBin@")
  (org-columns-default-format "%42ITEM %TODO %3Effort(E){:} %3CLOCKSUM_T(R) %SCHEDULED")
  (org-confirm-babel-evaluate nil)
  (org-confirm-elisp-link-function 'y-or-n-p)
  (org-confirm-elisp-link-function 'y-or-n-p)
  (org-confirm-elisp-link-not-regexp "org-tags-view")
  (org-confirm-shell-link-function 'y-or-n-p)
  (org-ctrl-k-protect-subtree t)
  (org-cycle-include-plain-lists 'integrate)
  (org-cycle-separator-lines 0)
  (org-deadline-warning-days 30)
  (org-default-notes-file "@emacsOrgDefaultNotesFile@")
  (org-ditaa-jar-path "@emacsOrgDitaaJarPath@")
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
  (org-loop-over-headlines-in-active-region t)
  (org-lowest-priority 70) ;; extend priorities set (given ascii code)
  (org-modules
   '(org-bookmark org-checklist org-collector
                  org-expiry org-id org-interactive-query
                  org-man org-protocol org-velocity)) ;TODO: find and review actual list
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
  (org-refile-use-outline-path 'file)
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
  (use-package org-capture-pop-frame :ensure t)
  ;;TODO: also add (as below) todo files from work projects (and maybe not only those)
  (f-entries "@emacsOrgDir@"
             (lambda (entry) (when (and (f-file? entry)
                                        (s-suffix? "org" entry)
                                        (not (s-contains? "journal" entry))
                                        (file-exists-p entry))
                               (push entry org-agenda-files))) t)
  ;; run some commands
  (org-add-link-type "tag" 'custom/follow-tag-link)
  (org-clock-persistence-insinuate)
  (run-with-idle-timer custom/idle-clockout-timeout t 'custom/clockout-when-idle)
  (turn-on-orgtbl))

(use-package org-bullets
  :ensure t
  :hook
  (org-mode-hook . org-bullets-mode))

;TODO: review https://github.com/abo-abo/org-download
(use-package org-download
  :ensure t
  ;;TODO: bind keys ASAP
  ;;TODO: use in automation
  :hook (dired-mode-hook . org-download-enable)
  :custom
  (org-download-method 'attach))

(use-package org-protocol
  :after org server)

(use-package org-rich-yank
  :ensure t
  :after org
  :bind
  (:map org-mode-map
        ("C-M-y" . org-rich-yank)))

(use-package orgit
  ;;TODO: automate insertion of links below (yasnippet/whatever)
  ;;    orgit:/path/to/repo/            links to a `magit-status' buffer
  ;;    orgit-rev:/path/to/repo/::REV   links to a `magit-revision' buffer
  ;;    orgit-log:/path/to/repo/::ARGS  links to a `magit-log' buffer
  :ensure t)

(use-package ox-html
  :ensure org-plus-contrib
  :commands (org-html-convert-region-to-html
             org-html-export-as-html
             org-html-export-to-html))
