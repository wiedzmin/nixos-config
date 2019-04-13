(use-package org
  :ensure org-plus-contrib
  :after (f)
  :mode (("\\.org$" . org-mode)
         ("\\.org_archive$" . org-mode))
  :preface
  ;; remove read-only props from yanked text (e.g. from jabber.el chat buffer)
  (defadvice org-yank (after make-yank-writeable disable)
    (let ((inhibit-read-only t))
      (remove-text-properties (region-beginning) (region-end)
                              '(read-only t))))
  (defvar custom/org-journal-file (at-org-dir "/journal.org"))
  (defvar custom/org-browser-tabs (at-org-dir "/browser-tabs.org"))
  (defun custom/verify-refile-target () ;; Exclude DONE state tasks from refile targets
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  ;;TODO: customize "todo-only" parameter for "org-tags-view"
  (defun custom/follow-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
         With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view nil tag))              ;nil was (null current-prefix-arg) originally
  ;; http://irreal.org/blog/?p=6166
  (defun custom/org-tags-all ()
    ;;TODO: bind some key to close buffer
    (interactive)
    (with-current-buffer (get-buffer-create "*org-tags*")
      (delete-region (point-min) (point-max))
      (org-mode)
      ;;TODO: review tag collection methods and find some truth
      ;; (sort (delete-dups (apply 'append (delete-dups (org-map-entries (lambda () org-scanner-tags) t 'agenda)))) 'string<)
      (let ((tags (sort (delete-dups
                         (cl-loop for buffer in (org-buffer-list 'agenda t)
                                  append (with-current-buffer buffer
                                           (org-with-wide-buffer
                                            (goto-char (point-min))
                                            (cl-loop while (re-search-forward org-complex-heading-regexp nil t)
                                                     when (match-string 5)
                                                     append (split-string (substring-no-properties (match-string 5))
                                                                          ":" t "[[:space:]]+"))))))
                        'string<)))
        (dolist (tag tags)
          (insert (concat "[[elisp:(org-tags-view nil \"" tag "\")][" tag "]]\n"))))
      (beginning-of-buffer)
      (switch-to-buffer (current-buffer))
      (read-only-mode)))
  ;; Remove empty CLOCK drawers on clock out
  (defun custom/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "CLOCK" (point))))
  (defun custom/org-use-speed-commands-for-headings-and-lists ()
    "Activate speed commands on list items too."
    (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
        (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
  ;; TODO: bind somewhere
  (defun custom/org-capture-refile-and-jump ()
    (interactive)
    (org-capture-refile)
    (org-refile-goto-last-stored))
  ;;TODO: investigate usage, seems useful
  (defun custom/org-link-describe (link desc)
    (cond ((string-match "file:" link)
           (replace-regexp-in-string "^file:" "File link -> " (org-link-unescape link)))
          (t (or desc link))))
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
  :general
  (:prefix "<f7>"
           "g" 'org-clock-goto
           "." 'org-clock-in
           "," 'org-clock-out
           "^" 'org-mru-clock-select-recent-task
           "c" 'org-clock-cancel
           "x" 'counsel-org-clock-context
           "h" 'counsel-org-clock-history
           "d" 'org-clock-display
           "R" 'org-clock-report
           "p" 'org-pomodoro
           "s" 'org-schedule
           "|" 'org-deadline
           "t" 'org-toggle-timestamp-type
           "e" 'org-capture
           "w" 'org-store-link
           "y" 'org-insert-link-global
           "S" 'org-set-property
           "D" 'org-delete-property
           "A" 'org-footnote-action
           "r" 'org-refile
           "T" 'org-table-create
           "a" 'org-agenda
           "b" 'org-dashboard-display
           "v" 'org-reveal
           "f" 'ace-link-org
           "n" 'org-narrow-to-subtree
           "-" 'org-sparse-tree
           "l" 'counsel-org-agenda-headlines
           "H" 'org-recent-headings-ivy
           "=" 'org-show-todo-tree
           "\\" 'counsel-org-tag
           "<right>" 'outline-next-visible-heading
           "<left>" 'outline-previous-visible-heading
           "<down>" 'org-forward-heading-same-level
           "<up>" 'org-backward-heading-same-level
           "u" 'outline-up-heading
           "G" 'org-goto
           ";" 'custom/org-tags-all)
  ;; (:keymaps 'org-agenda-mode-map
  ;;           "<f7> ." 'org-agenda-clock-in org-agenda-mode-map
  ;;           "<f7> ," 'org-agenda-clock-out org-agenda-mode-map
  ;;           "<f7> o" 'ace-link-org org-agenda-mode-map)
  (:keymaps 'org-mode-map
            "C-'" nil
            "C-c [" nil
            "C-c ]" nil
            "C-c C-o" nil
            "s-j" 'org-babel-next-src-block
            "s-k" 'org-babel-previous-src-block
            "s-l" 'org-edit-src-code
            "C-c C-'" 'org-edit-src-code)
  (:keymaps 'org-src-mode-map
            "s-l" 'org-edit-src-exit
            "C-c C-'" 'org-edit-src-exit)

    :config
  (use-package org-capture-pop-frame :ensure t)
  (setq org-archive-location (concat custom/org-journal-file "::datetree/"))
  (setq org-contrib-base '(org-agenda org-archive org-attach org-bbdb
                           org-bibtex org-clock org-docview org-habit
                           org-id org-info org-inlinetask org-irc
                           org-mouse org-protocol org-timer org-w3m))
  (setq org-contrib-extra '(org-bookmark org-checklist org-collector
                            org-drill org-expiry org-index org-interactive-query
                            org-man org-velocity))
  (setq org-modules `(,@org-contrib-base ,@org-contrib-extra))
  (add-to-list 'file-coding-system-alist (cons "\\.\\(org\\|org_archive\\|/TODO\\)$"  'utf-8))
  (setq org-lowest-priority 70) ;; extend priorities set (given ascii code)
  (setq org-use-speed-commands 'custom/org-use-speed-commands-for-headings-and-lists)
  (setq org-use-speed-commands t)
  (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
  (f-entries (at-org-dir)
             (lambda (entry) (when (and (f-file? entry)
                                        (s-suffix? "org" entry)
                                        (file-exists-p entry))
                               (push entry org-agenda-files)))
             t)
  (dolist (orgfile (directory-files (at-org-dir "/journals") t "journal") )
    (setq org-agenda-files
          (delete orgfile org-agenda-files)))
  (add-to-list 'org-agenda-files (at-config-basedir "config.org"))
  ;; agenda customizations
  (setq org-confirm-elisp-link-not-regexp "org-tags-view")
  (setf org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :narrow 60))
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-agenda-include-all-todo t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-persistent-filter t)
  (setq org-agenda-repeating-timestamp-show-all nil)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-inherited-tags nil)
  (setq org-agenda-show-log t)
  (setq org-agenda-skip-additional-timestamps-same-entry t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-span 'month)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-sticky nil) ;otherwise agenda behaves strangely on non-stuck projects
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-todo-ignore-deadlines 'all)
  (setq org-agenda-todo-ignore-scheduled 'all)
  (setq org-agenda-todo-ignore-timestamp 'past)
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-list-sublevels nil)
  (setq org-agenda-use-tag-inheritance t)
  (setq org-agenda-window-setup 'current-window)
  (setf agenda-opts-all-with-time
        '((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)))
  (setq org-agenda-time-grid
        '((daily today require-timed remove-match)
          "----------------"
          (930 1000 1200 1400 1600 1800 2000 2200 2400 2500)))
  (setq org-agenda-custom-commands
        `(("d" . "some non-straightforward TODO statuses")
          ("db" todo "BACKLOG" nil)
          ("ds" todo "SOON" nil)
          ("dc" todo "CANCELLED" nil)
          ("dw" todo "WAITING|FEEDBACK" nil)
          ("dg" todo "GOING" ,agenda-opts-all-with-time)
          ("da" tags "+actual_p")
          ("c" . "by context")
          ("cp" tags "+@personal/GOING|WAITING|BACKLOG|SOON")
          ("cr" tags "+@project/GOING|WAITING|BACKLOG|SOON")
          ("cj" tags "+@job/GOING|WAITING|FEEDBACK|BACKLOG|SOON")
          ("cw" tags "+@workplace/GOING|WAITING|BACKLOG|SOON")
          ("ct" tags "+@phonecall/WAITING|BACKLOG|SOON")
          ("cs" tags "+@someday")
          ("cq" tags "+@quicknote")
          ("e" . "by essence")
          ;;TODO: find more handy shortcuts
          ("ec" tags "+current")
          ("ef" tags "+reference")
          ("em" tags "+master")
          ("eo" tags "+ordering")
          ("er" tags "+repair")
          ("ed" tags "+develop")
          ("ei" tags "+investigate")
          ("ee" tags "+entertainment")
          ("ey" tags "+family")
          ("eH" tags-todo "+housekeeping")
          ("eC" tags-todo "+current")
          ("eF" tags-todo "+reference")
          ("eM" tags-todo "+master")
          ("eO" tags-todo "+ordering")
          ("eR" tags-todo "+repair")
          ("eD" tags-todo "+develop")
          ("eI" tags-todo "+investigate")
          ("eE" tags-todo "+entertainment")
          ("u" . "unassigned")
          ("up" alltodo "Unprioritized TODO entries"
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'regexp "\\[#[ABC]]")))
            (org-tags-match-list-sublevels 'indented)
            (org-agenda-sorting-strategy
             '((agenda time-up tag-up) ))
            ;; '(org-agenda-sorting-strategy '((agenda time-up priority-down tag-up) (todo tag-up)))
            (org-agenda-overriding-header "Unprioritized TODO entries: ")))
          ("P" . "Prioritized tasks")
          ("Pa" "Prioritized tasks A"
           ((tags-todo "+PRIORITY=\"A\"") ))
          ("Pb" "Prioritized tasks B"
           ((tags-todo "+PRIORITY=\"B\"")))
          ("Pc" "Prioritized tasks C"
           ((tags-todo "+PRIORITY=\"C\"")))
          ("S" "Scheduled tasks" agenda ""
           ((org-agenda-time-grid nil)
            (org-deadline-warning-days 32)
            (org-agenda-entry-types '(:scheduled))
            ))
          ("p" tags "+purchase")
          ("b" . "tickets")
          ("be" tags "+ticket+emacs")
          ("bs" tags "+ticket+stumpwm")
          ("jc" tags "+@job+current/GOING|FEEDBACK")
          ))
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s %b")
                                   (timeline . "  % s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))
  ;; clocking customizations
  (setq org-clock-history-length 35)
  (setq org-clock-idle-time 3)
  (setq org-clock-in-resume t)
  (setq org-clock-in-switch-to-state "GOING")
  (setq org-clock-out-switch-to-state "HOLD")
  (setq org-clock-into-drawer "CLOCK")
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-persist t)
  ;; just clock-out unconditionally - it seems easier to maintain (credits to @binarin)
  (setf org-clock-x11idle-program-name "xprintidle")
  (setf org-x11idle-exists-p t)
  ;; refiling customizations
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-target-verify-function 'custom/verify-refile-target)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))
  (setq org-refile-use-outline-path 'file)
  ;; various customizations
  (setf org-catch-invisible-edits nil)
  (setf org-fast-tag-selection-include-todo nil)
  (setf org-id-link-to-org-use-id t)
  (setq appt-display-interval 5)
  (setq appt-message-warning-time 10)
  (setq calendar-date-style 'european)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-align-all-tags t)
  (setq org-attach-directory (at-org-dir "/org-attach-data"))
  (setq org-blank-before-new-entry '((heading) (plain-list-item . auto)))
  (setq org-columns-default-format "%42ITEM %TODO %3Effort(E){:} %3CLOCKSUM_T(R) %SCHEDULED")
  (setq org-confirm-elisp-link-function 'y-or-n-p)
  (setq org-ctrl-k-protect-subtree t)
  (setq org-cycle-include-plain-lists 'integrate)
  (setq org-cycle-separator-lines 0)
  (setq org-deadline-warning-days 30)
  (setq org-default-notes-file (at-org-dir "/refile.org"))
  (setq org-ditaa-jar-path (at-config-basedir "resources/ditaa0_9.jar"))
  (setq org-element-use-cache nil)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)  ;;TODO: try ORDERED/NOBLOCKING props : org-toggle-ordered-property
  (setq org-export-coding-system 'utf-8)
  (setq org-export-with-drawers t)
  (setq org-extend-today-until 2)
  (setq org-fast-tag-selection-single-key 'expert)
  (setq org-fontify-done-headline t)
  (setq org-global-properties '(("STYLE_ALL" . "habit")))
  (setq org-goto-max-level 10)
  (setq org-hide-leading-stars t)
  (setq org-indirect-buffer-display 'current-window)
  (setq org-insert-mode-line-in-empty-file t)
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-log-repeat 'time)
  (setq org-loop-over-headlines-in-active-region t)
  (setq org-read-date-prefer-future 'time)
  (setq org-return-follows-link t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-folded nil)
  (setq org-stuck-projects '("+LEVEL=1/-DONE" ("TODO" "GOING" "NEXT" "WAITING" "HOLD" "CANCELLED") nil ""))
  (setq org-tags-column -80)
  (setq org-track-ordered-property-with-tag t)
  (setq org-use-effective-time t)
  (setq org-use-property-inheritance t)
  (setq org-use-sub-superscripts nil)
  (setq org-yank-adjusted-subtrees t)
  (setq org-agenda-show-future-repeats 'next)
  (setq org-highlight-latex-and-related '(latex))
  (setq org-confirm-shell-link-function 'y-or-n-p)
  (setq org-confirm-elisp-link-function 'y-or-n-p)
  (setq org-src-window-setup 'current-window)
  (setq org-confirm-babel-evaluate nil)
  (setf org-make-link-description-function #'custom/org-link-describe)
  (when (featurep 'unicode-fonts)
    (setq org-ellipsis "⤵"))
  ;; keywords setup
  (setq kw-seq-common '(sequence "BACKLOG(b)" "SOON(s)" "REPEAT(r)" "GOING(g!)" "NEXT(x)" "WAITING(w@/!)" "FEEDBACK"
                                 "|" "DONE(d!/@)" "CANCELLED(c@/!)" "OUTDATED(o)"))
  (setq org-todo-keywords
        `(,kw-seq-common))
  (setq org-todo-keywords-for-agenda '("BACKLOG(b)" "SOON(s)" "REPEAT(r)" "GOING(g!)" "NEXT(x)" "WAITING(w@/!)" "FEEDBACK"))
  (setq org-done-keywords-for-agenda '("DONE(d)" "CANCELLED(c)" "OUTDATED(o)"))
  ;; faces
  (setq org-todo-keyword-faces
        '(("BACKLOG" . (:foreground "gray" :weight bold))
          ("SOON" . (:foreground "magenta" :weight bold))
          ("REPEAT" . (:foreground "blue" :weight bold))
          ("NEXT" . (:foreground "red" :weight bold))
          ("WAITING" . (:foreground "orange" :weight bold))
          ("FEEDBACK" . (:foreground "yellow" :weight bold))
          ("CANCELLED" . (:foreground "cyan" :weight bold))
          ("DONE" . (:foreground "green" :weight bold))))
  (setq org-priority-faces
        '((?A :foreground "red" :weight bold)
          (?B :foreground "#94bff3" :weight bold)
          (?C :foreground "#6f6f6f")
          (?D :foreground "#c390d4")
          (?E :foreground "#90c3d4")
          (?F :foreground "#a1d490")))
  (set-face-attribute 'org-done nil :foreground "PaleGreen" :weight 'normal :strike-through t)
  (set-face-attribute 'org-headline-done nil :foreground "LightSalmon" :weight 'normal :strike-through t)
  ;; tags
  (setq org-tag-alist '(("current" . ?c)
                        ("reference" . ?f)
                        ("orgmode" . ?g)
                        ("purchase" . ?p)
                        ("master" . ?m)
                        ("ordering" . ?o)
                        ("housekeeping" . ?h)
                        ("entertainment" . ?e)
                        ("interesting" . ?i)
                        ("repair" . ?r)
                        ))
  (setq org-tags-exclude-from-inheritance '("project"))
  (setq org-todo-state-tags-triggers
        '(("GOING" ("current" . t))
          ("DONE" ("current"))))
  ;; org-habit
  (setq org-habit-graph-column 50)
  (setq org-habit-preceding-days 10)
  (setq org-habit-following-days 4)
  (setq org-habit-show-habits-only-for-today nil)
  ;; org-capture
  (setq org-capture-templates
        `(("n" "NixOS")
          ("nt" "NixOS" entry (file "/etc/nixos/todo.org") "* BACKLOG %?[[%:link][%:description]] %U\n  %:initial")
          ("nc" "NixOS code snippet" entry (file "/etc/nixos/todo.org")
           "* %^{title} :nix:code_snippet:\n :PROPERTIES:\n :CREATED: %U\n :END:\n\n#+BEGIN_SRC nix\n %i%?\n#+END_SRC\n")
          ("ns" "NixOS shell excerpt" entry (file "/etc/nixos/todo.org") "* %? %U :%:description:\n  %:initial")
          ("e" "Emacs")
          ("et" "Emacs" entry (file ,(at-config-basedir "/todo.org")) "* BACKLOG %?[[%:link][%:description]] %U\n  %:initial")
          ("ec" "Emacs code snippet" entry (file ,(at-config-basedir "/todo.org"))
           "* %^{title} :emacs:code_snippet:\n :PROPERTIES:\n :CREATED: %U\n :END:\n\n#+BEGIN_SRC emacs-lisp\n %i%?\n#+END_SRC\n")
          ("es" "NixOS shell excerpt" entry (file ,(at-config-basedir "/todo.org")) "* %? %U :%:description:\n  %:initial")
          ("x" "XMonad")
          ("xt" "XMonad" entry (file ,(at-homedir "/.xmonad/todo.org")) "* BACKLOG %?[[%:link][%:description]] %U\n  %:initial")
          ("xc" "XMonad code snippet" entry (file ,(at-homedir "/.xmonad/todo.org"))
           "* %^{title} :xmonad:code_snippet:\n :PROPERTIES:\n :CREATED: %U\n :END:\n\n#+BEGIN_SRC haskell\n %i%?\n#+END_SRC\n")
          ("xs" "XMonad shell excerpt" entry (file ,(at-homedir "/.xmonad/todo.org")) "* %? %U :%:description:\n  %:initial")
          ("d" "deferred tabs" entry (file+olp custom/org-browser-tabs "groups" "deferred tabs") "* %?%:link %U :deferred:")
          ("p" "projects")
          ("pi" "project ideas" entry (file ,(at-org-dir "/projects.org")) "* %? %U :@project:idea:")
          ("pn" "new project" entry (file ,(at-org-dir "/projects.org")) "* %? %U :@project:")
          ("m" "mastering" entry (file+headline ,(at-org-dir "/mastering.org") "inbox") "* %? %U")
          ))
  ;; holidays
  (setq holiday-orthodox-holidays nil) ; Orthodox holidays to some extent
  (setq holiday-personal-holidays nil) ; personal anniversaries, etc.
  (setq holiday-other-holidays
        (append holiday-orthodox-holidays holiday-personal-holidays))
  (setq calendar-holidays
        (append holiday-other-holidays
                holiday-solar-holidays))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-clock-out-hook 'custom/remove-empty-drawer-on-clock-out 'append)
  (add-hook 'org-after-refile-insert-hook 'save-buffer)
  (add-hook 'org-capture-mode-hook
            (lambda () (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
  ;; run some commands
  (org-add-link-type "tag" 'custom/follow-tag-link)
  (org-clock-persistence-insinuate) ;; Resume clocking tasks when emacs is restarted
  (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))
  (set-charset-priority 'unicode)
  (turn-on-orgtbl)
  (run-with-idle-timer custom/idle-clockout-timeout t 'custom/clockout-when-idle)
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append))

(use-package org-protocol
  :after (org server))

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

(use-package ox-html
  :ensure org-plus-contrib
  :commands (org-html-convert-region-to-html
             org-html-export-as-html
             org-html-export-to-html))

(use-package ob-async
  :ensure t
  :after (org ob))

(use-package ob-restclient
  :ensure t
  :after (ob restclient)
  :commands (org-babel-execute:restclient))

(use-package org-pomodoro
  :ensure t
  :after (alert))

(use-package orgit
  ;;TODO: automate insertion of links below (yasnippet/whatever)
  ;;    orgit:/path/to/repo/            links to a `magit-status' buffer
  ;;    orgit-rev:/path/to/repo/::REV   links to a `magit-revision' buffer
  ;;    orgit-log:/path/to/repo/::ARGS  links to a `magit-log' buffer
  :ensure t)

(use-package orglink
  :ensure t
  :delight (orglink-mode " OL")
  :config
  ;; TODO: customize orglink-activate-in-modes
  ;; TODO: automate insertion of link types below
  ;;   [[Code]]
  ;;   [[Code][start of code]]
  ;;   [[define-derived-mode orglink-mode][orglink-mode]]
  ;;   <mailto:jonas@bernoul.li>
  ;;   man:info
  ;;   <info:man>
  ;;   https://github.com/tarsius/orglink
  (global-orglink-mode))

(use-package org-clock-today
  :ensure t
  :config
  (org-clock-today-mode 1))

(use-package org-recent-headings
  :ensure t
  :disabled
  :custom
  (org-recent-headings-save-file (at-user-data-dir "org-recent-headings"))
  :config
  (org-recent-headings-mode 1))

(use-package org-link-minor-mode
  :ensure t
  :config
  (org-link-minor-mode t))

(use-package org-randomnote
  :ensure t
  :general
  (:keymaps 'mode-specific-map
            "o r" 'org-randomnote)
  :custom
  (org-randomnote-candidates org-agenda-files)
  (org-randomnote-open-behavior 'indirect-buffer))

(use-package russian-holidays
  :ensure t
  :after (org)
  :config
  (setq calendar-holidays
   (push russian-holidays calendar-holidays)))

(use-package org-rich-yank
  :ensure t
  :after (org)
  :general
  (:keymaps 'org-mode-map
            "C-M-y" 'org-rich-yank))

(use-package counsel-org-clock
  :ensure t
  :after (org counsel)
  :custom
  (counsel-org-clock-default-action 'counsel-org-clock-clock-dwim-action))

(use-package org-download
  :ensure t
  ;;TODO: bind keys ASAP
  ;;TODO: use in automation
  :hook (dired-mode-hook . org-download-enable)
  :custom
  (org-download-method 'attach))

(use-package blockdiag-mode :ensure t)

(use-package ob-blockdiag
  :ensure t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((blockdiag . t))))

;; try https://github.com/abrochard/org-sync-snippets
