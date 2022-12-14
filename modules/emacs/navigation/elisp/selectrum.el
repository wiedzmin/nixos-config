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
  (selectrum-quick-keys-highlight ((t (:foreground "red"))))
  (selectrum-quick-keys-match ((t (:foreground "green"))))
  :config
  (selectrum-mode +1))

(use-package consult-selectrum) ;TODO: check if it is loaded early enough to handle its assignment properly

(with-eval-after-load 'orderless
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)) ;; perf tip: highlight only visible candidates
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

(with-eval-after-load 'marginalia
  ;; Default command category to 'marginalia-annotate-binding instead of
  ;; 'marginalia-annotate-command which has a slight performance impact when
  ;; filtering M-x candidates.
  (mapc
   (lambda (x)
     (pcase (car x) ('command (setcdr x (cons 'marginalia-annotate-binding
                                              (remq 'marginalia-annotate-binding (cdr x)))))))
   marginalia-annotator-registry)
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit 'keep-selected)))))
