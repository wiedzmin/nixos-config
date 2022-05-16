(use-package codesearch
  :bind
  (:map custom-projectile-map
        ("c" . listing-codesearch-search))
  :custom
  (codesearch-global-csearchindex "@globalWorkspaceRoot@/.csearchindex"))
