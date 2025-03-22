(use-package emacs-lisp-ts-mode
  :load-path "@emacsElispTsModePath@"
  :mode ("\\.el$" . emacs-lisp-ts-mode)
  :bind
  (:map emacs-lisp-ts-mode-map
        ("C-c C-d C-d" . describe-function)
        ("C-c C-d d" . describe-function)
        ("C-c C-k" . eval-buffer)))
