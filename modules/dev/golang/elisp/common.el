(use-package ob-go
  :commands (org-babel-execute:go
             org-babel-expand-body:go
             org-babel-prep-session:go))

(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)
                           (matcher . "CaseSensitive"))))))
