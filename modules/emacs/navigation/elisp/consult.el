;; TODO: consider trying `fd' finder tool, see https://github.com/minad/consult/wiki#find-files-using-fd for reference
;; Note: this requires lexical binding
(use-package consult
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
  (consult-line-numbers-widen t)
  (consult-narrow-key "<")
  (consult-preview-key '(:debounce 0.5 any))
  (register-preview-delay 0.1)
  (register-preview-function #'consult-register-format)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-recent-file :preview-key '("S-<up>" "S-<down>"))
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'register-preview :override #'consult-register-window)
  (consult-customize consult-line :keymap custom/consult-line-map)
  (dolist (func '(consult-git-grep consult-ripgrep consult-grep))
    (advice-add func :before
                (defun custom/mark-jump-point (&rest _)
                  (xref-push-marker-stack)
                  (push-mark))))
  (define-minibuffer-key "\C-s"
                         'consult-location #'previous-history-element
                         'file #'consult-find-for-minibuffer)
  (fset 'multi-occur #'consult-multi-occur))

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "`" 'consult-file-externally))

(with-eval-after-load 'bookmark-view
  (setq consult-view-open-function #'bookmark-jump)
  (setq consult-view-list-function #'bookmark-view-names)
  (add-to-list 'consult-buffer-sources
               (list :name     "View"
                     :narrow   ?v
                     :category 'bookmark
                     :face     'font-lock-keyword-face
                     :history  'bookmark-view-history
                     :action   #'consult--bookmark-action
                     :items    #'bookmark-view-names)
               'append))

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
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

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

(use-package consult-flycheck
  :after (consult flycheck)
  :bind
  (:map mode-specific-map
        ("y" . consult-flycheck))
  (:map flycheck-mode-map
        ("C-c ! o" . consult-flycheck)
        ("!" . consult-flycheck)))

(with-eval-after-load 'pulsar
  (mapc (lambda (x) (add-hook 'consult-after-jump-hook x)) `(#'pulsar-recenter-top #'pulsar-reveal-entry)))

(with-eval-after-load 'dogears
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
  (add-to-list 'consult-buffer-sources consult--source-dogears 'append)
  (defun consult-dogears ()
    (interactive)
    (consult--multi '(consult--source-dogears))))

(with-eval-after-load 'bufler
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
  (add-to-list 'consult-buffer-sources consult--bufler-workspace+ 'append))

(with-eval-after-load 'vertico-multiform
  (setq vertico-multiform-commands '((consult-line buffer)
                                     (consult-imenu reverse buffer))))

(eval-after-load 'xref
  (use-package xref
    :custom
    (xref-show-xrefs-function #'consult-xref)
    (xref-show-definitions-function #'consult-xref)))
