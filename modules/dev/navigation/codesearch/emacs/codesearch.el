(use-package codesearch
  :custom
  (codesearch-global-csearchindex "@globalWorkspaceRoot@/.csearchindex"))

(use-package projectile-codesearch
  :after codesearch
  :bind
  (:map custom-projectile-map
         ("c" . projectile-codesearch-search)))
