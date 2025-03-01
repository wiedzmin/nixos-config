(use-package python
  :mode ("\\.py$" . python-mode)
  :hook
  (python-mode-hook . (lambda ()
                        (setq indent-tabs-mode nil)
                        (setq tab-width 4)
                        (auto-fill-mode 1)))
  ;; Highlight the call to ipdb, src http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
  (python-mode-hook . (lambda ()
                        (highlight-lines-matching-regexp "import ipdb")
                        (highlight-lines-matching-regexp "ipdb.set_trace()")
                        (highlight-lines-matching-regexp "import wdb")
                        (highlight-lines-matching-regexp "wdb.set_trace()")))
  (python-mode-hook . @lspStartFunction@)
  (python-mode-hook . flycheck-mode)
  @lspModePythonRegisterServer@
  :bind
  (:map python-mode-map
        ("C-k" . sp-kill-hybrid-sexp)
        ("M-_" . python-indent-shift-left)
        ("M-+" . python-indent-shift-right))
  :custom
  (python-indent-block-paren-deeper t))

(with-eval-after-load 'focus
  (with-eval-after-load 'python
    (add-to-list 'focus-mode-to-thing '(python-mode . paragraph))))
