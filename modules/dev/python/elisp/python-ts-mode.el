(use-package python
  :mode ("\\.py$" . python-ts-mode)
  :hook
  (python-ts-mode-hook . (lambda ()
                           (setq indent-tabs-mode nil)
                           (setq tab-width 4)
                           (auto-fill-mode 1)))
  ;; Highlight the call to ipdb, src http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
  (python-ts-mode-hook . (lambda ()
                           (highlight-lines-matching-regexp "import ipdb")
                           (highlight-lines-matching-regexp "ipdb.set_trace()")
                           (highlight-lines-matching-regexp "import wdb")
                           (highlight-lines-matching-regexp "wdb.set_trace()")))
  (python-ts-mode-hook . @lspStartFunction@)
  (python-ts-mode-hook . flycheck-mode)
  @lspModePythonTSRegisterServer@
  :bind
  (:map python-ts-mode-map
        ("C-k" . sp-kill-hybrid-sexp)
        ("C-<down>" . sp-push-hybrid-sexp)
        ("C-<right>" . sp-slurp-hybrid-sexp)
        ("M-_" . python-indent-shift-left)
        ("M-+" . python-indent-shift-right)))

(with-eval-after-load 'focus
  (add-to-list 'focus-mode-to-thing '(python-ts-mode . paragraph)))
