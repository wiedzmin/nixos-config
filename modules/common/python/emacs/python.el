(use-package lsp-python-ms
  :custom
  (lsp-python-ms-executable "@lspPythonMsExecutable@")
  (lsp-python-ms-extra-paths [@lspPythonMsExtraPaths@]))

(use-package python
  :mode ("\\.py$" . python-mode)
  :hook
  (python-mode-hook . (lambda ()
                        (setq indent-tabs-mode nil)
                        (setq tab-width 4)
                        (setq imenu-create-index-function 'imenu-default-create-index-function)
                        (auto-fill-mode 1)))
  ;; Highlight the call to ipdb, src http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/
  (python-mode-hook . (lambda ()
                        (highlight-lines-matching-regexp "import ipdb")
                        (highlight-lines-matching-regexp "ipdb.set_trace()")
                        (highlight-lines-matching-regexp "import wdb")
                        (highlight-lines-matching-regexp "wdb.set_trace()")))
  (python-mode-hook . lsp-deferred)
  (python-mode-hook . flycheck-mode)
  :bind
  (:map python-mode-map
        ("C-k" . sp-kill-hybrid-sexp)
        ("C-<down>" . sp-push-hybrid-sexp)
        ("C-<right>" . sp-slurp-hybrid-sexp)
        ("M-_" . python-indent-shift-left)
        ("M-+" . python-indent-shift-right))
  :capf python-completion-at-point
  :config
  (add-function :before-until (local 'eldoc-documentation-function)
                #'(lambda () "")))

(use-package flycheck-prospector
  :after flycheck)

(use-package pip-requirements
  :delight
  :preface
  (defun custom/pip-requirements-ignore-case ()
    (setq-local completion-ignore-case t))
  :mode ("requirements\\." . pip-requirements-mode)
  :hook (pip-requirements-mode . custom/pip-requirements-ignore-case))
