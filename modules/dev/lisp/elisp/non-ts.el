(use-package elisp-mode
  :mode ("\\.el$" . emacs-lisp-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-d C-d" . describe-function)
        ("C-c C-d d" . describe-function)
        ("C-c C-k" . eval-buffer)))
