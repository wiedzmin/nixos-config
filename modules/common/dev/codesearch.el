(use-package codesearch
  :ensure t
  :custom
  (codesearch-global-csearchindex "@devWorkspaceRoot@/.csearchindex"))

(use-package counsel-codesearch
  :ensure t
  :after codesearch
  :bind
  (:map mode-specific-map
        ("c" . counsel-codesearch)))

(use-package projectile-codesearch
  :ensure t
  :after codesearch)
