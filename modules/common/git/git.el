(use-package git-timemachine
  :bind
  (:map mode-specific-map
        (";" . git-timemachine)))

(use-package helm-ghq
  :bind
  (:map custom-nav-map
        ("h" . helm-ghq)))

(use-package magit
  :after helm
  :mode (("COMMIT_EDITMSG" . conf-javaprop-mode)
         ("COMMIT" . git-commit-mode))
  :bind
  (:prefix-map custom-magit-map
               :prefix "C-'"
               ("B" . magit-branch)
               ("L" . magit-reflog-current)
               ("O" . magit-reflog-other)
               ("R" . magit-rebase)
               ("S" . magit-stash)
               ("U" . magit-update-index)
               ("a" . magit-stage-file)
               ("b" . magit-blame-addition) ; TODO: add for *-removal
               ("c" . magit-checkout)
               ("d" . magit-diff)
               ("f" . magit-log-buffer-file)
               ("i" . magit-init)
               ("l" . magit-log)
               ("n" . magit-notes-edit)
               ("r" . magit-reset)
               ("s" . magit-status)
               ("t" . magit-tag)
               ("w" . magit-diff-working-tree))
  (:map magit-status-mode-map
        ("N" . magit-notes-edit)
        ("C-c k" . magit-process-kill))
  (:map dired-mode-map
        ("@" . magit-dired-log))
  :preface
  (defun custom/toggle-magit-margin ()
    (interactive)
    (if (car magit-status-margin)
        (setq magit-status-margin '(nil age magit-log-margin-width nil 18))
      (setq magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))))
  :custom
  (magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (magit-blame-heading-format "%H %-20a %C %s")
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-filenotify
  :delight " FN"
  :hook (magit-status-mode-hook . (lambda ()
                                    (condition-case nil
                                        (magit-filenotify-mode)
                                      (error (magit-filenotify-mode -1))))))

(use-package git-walktree
  :after magit
  :bind
  (:map custom-magit-map
        ("o" . git-walktree)))

(use-package git-identity
 :after magit
 :bind
 (:map magit-status-mode-map
       ("I" . git-identity-info))
 :config
 (git-identity-magit-mode 1)
 :custom
 (git-identity-verify t)
 (git-identity-default-username "@mainUserName@"))

(use-package dired-git-info
  :after dired
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

(use-package git-msg-prefix
  :bind
  (:map git-commit-mode-map
        ("C-c i" . git-msg-prefix))
  :custom
  (git-msg-prefix-regex
   (rx bol (group (one-or-more
                   (group (zero-or-more alnum) ":" space)))))
  (git-msg-prefix-log-flags " --since='1 week ago' ")
  (git-msg-prefix-input-method 'helm-comp-read))

(use-package magit-todos
  :after (magit projectile helm)
  :bind
  (:map mode-specific-map
        ("C-d" . helm-magit-todos))
  (:map custom-projectile-map
        ("t" . helm-magit-todos)))

(use-package git-link
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . git-link)
        ("c" . git-link-commit))
  (:map magit-status-mode-map
        ("o" . git-link)
        ("H" . git-link-commit))
  :custom
  (git-link-open-in-browser t)
  (git-link-use-commit t))

(use-package smerge-mode
  :delight " ∓"
  :bind
  (:map mode-specific-map
        ("g k" . smerge-prev)
        ("g j" . smerge-next))
  :hook (find-file-hooks . (lambda ()
                             (save-excursion
                               (goto-char (point-min))
                               (when (re-search-forward "^<<<<<<< " nil t)
                                 (smerge-mode 1))))))
