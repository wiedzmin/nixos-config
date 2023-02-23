(use-package lisp
  :hook
  (after-save . check-parens))

(use-package elisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-d C-d" . describe-function)
        ("C-c C-d d" . describe-function)
        ("C-c C-k" . eval-buffer)))

(use-package highlight-defined
  :custom
  (highlight-defined-face-use-itself t)
  :hook
  (help-mode . highlight-defined-mode)
  (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(use-package highlight-sexp
  :load-path "@emacsHighlightSexpPath@"
  :hook
  (clojure-mode . highlight-sexp-mode)
  (emacs-lisp-mode . highlight-sexp-mode)
  (lisp-mode . highlight-sexp-mode))

(use-package eros
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package suggest
  :defer t)

(use-package ipretty
  :defer t
  :config
  (ipretty-mode 1))

(use-package nameless
  :disabled
  :hook
  (emacs-lisp-mode .  nameless-mode)
  :custom
  (nameless-global-aliases '())
  (nameless-private-prefix t))

(use-package erefactor
  :defer t)

(use-package flycheck-package
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

(use-package elsa
  :defer t)

(use-package flycheck-elsa
  :hook
  (emacs-lisp-mode . flycheck-elsa-setup))

;; https://github.com/AmaiKinono/puni
