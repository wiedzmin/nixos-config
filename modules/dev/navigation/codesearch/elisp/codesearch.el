(use-package codesearch
  :bind
  (:map custom-projects-map
        ("c" . listing-codesearch-search))
  :custom
  (codesearch-global-csearchindex "@globalWorkspaceRoot@/.csearchindex"))
