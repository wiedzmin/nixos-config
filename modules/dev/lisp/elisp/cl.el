;FIXME: fails to start
(use-package slime ;NOTE: from melpa-stable, because it should correspond to quicklisp version
  :hook ((lisp-mode-hook . (lambda ()
                             (slime-mode t)
                             (set (make-local-variable 'slime-lisp-implementations)
                                  (list (assoc 'sbcl slime-lisp-implementations)))))
         (inferior-lisp-mode-hook . inferior-slime-mode)
         ;; (slime-mode-hook . (lambda () (when (> emacs-major-version 25)
         ;;                                 (slime-autodoc-mode -1)))) ;; some signature down the call stack is broken in 2.20
         (lisp-mode-hook . (lambda ()
                             (auto-fill-mode 1)
                             (setq indent-tabs-mode nil))))
  :init
  (use-package slime-autoloads)
  :custom
  (slime-complete-symbol*-fancy t)
  (slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (slime-net-coding-system 'utf-8-unix)
  :config
  (defadvice slime-documentation-lookup
      (around change-browse-url-browser-function activate)
    "Use w3m for slime documentation lookup."
    (let ((browse-url-browser-function 'w3m-browse-url))
      ad-do-it))
  (slime-setup
   '(slime-fancy-inspector
     slime-fancy-trace
     slime-fontifying-fu
     ;; slime-hyperdoc
     slime-package-fu
     slime-references
     slime-trace-dialog
     slime-xref-browser
     slime-asdf
     ;; slime-autodoc
     slime-banner
     slime-fancy
     slime-fuzzy
     slime-repl
     slime-sbcl-exts
     ))
  (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")  :coding-system utf-8-unix))
  )

(use-package slime-company
  :after (slime company))

(use-package inf-lisp
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package common-lisp-snippets
  @clsDisabled@)
