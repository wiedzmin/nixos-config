(use-package codesearch
  :custom
  (codesearch-global-csearchindex "@devWorkspaceRoot@/.csearchindex"))

(use-package counsel-codesearch
  :after codesearch
  :bind
  (:map mode-specific-map
        ("c" . counsel-codesearch)))

(use-package projectile-codesearch
  :after codesearch)
