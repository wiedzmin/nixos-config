(use-package ms-pyls-client
  :quelpa
  (ms-pyls-client :repo "wiedzmin/ms-pyls-client" :fetcher github)
  :custom
  (lsp-python-ms-extra-paths '(@lspPythonMsExtraPaths@)))

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
  (python-mode-hook . lsp)
  (python-mode-hook . flycheck-mode)
  :bind
  (:map python-mode-map
        ("M-_" . python-indent-shift-left)
        ("M-+" . python-indent-shift-right))
  :config
  (add-function :before-until (local 'eldoc-documentation-function)
                #'(lambda () "")))

(use-package py-yapf :ensure t)

(use-package flycheck-prospector
  :ensure t
  :after flycheck)

(use-package pip-requirements
  :ensure t
  :delight (pip-requirements-mode "PyPA Requirements")
  :preface
  (defun custom/pip-requirements-ignore-case ()
    (setq-local completion-ignore-case t))
  :mode ("requirements\\." . pip-requirements-mode)
  :hook (pip-requirements-mode . custom/pip-requirements-ignore-case))
