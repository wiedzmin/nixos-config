(use-package flycheck-prospector
  :after flycheck)

(use-package pip-requirements
  :delight
  :preface
  (defun custom/pip-requirements-ignore-case ()
    (setq-local completion-ignore-case t))
  :mode ("requirements\\." . pip-requirements-mode)
  :hook (pip-requirements-mode . custom/pip-requirements-ignore-case))

(with-eval-after-load 'python
  (with-eval-after-load 'company
    (when (boundp 'company-backends)
      (add-to-list 'company-backends 'company-tabnine)
      (add-to-list 'company-backends 'company-capf)))
  (add-to-list 'completion-at-point-functions #'pcomplete-completions-at-point)
  (add-function :before-until (local 'eldoc-documentation-function)
                #'(lambda () "")))
