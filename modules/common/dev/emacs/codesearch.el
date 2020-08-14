(use-package codesearch
  :custom
  (codesearch-global-csearchindex "@globalWorkspaceRoot@/.csearchindex"))

(use-package helm-codesearch
  :after codesearch
  :bind
  (:map mode-specific-map
        ("h f" . helm-codesearch-find-file)
        ("h t" . helm-codesearch-find-pattern))
  :custom
  (helm-codesearch-overwrite-search-result t)
  (helm-codesearch-global-csearchindex "@globalWorkspaceRoot@/.csearchindex"))
