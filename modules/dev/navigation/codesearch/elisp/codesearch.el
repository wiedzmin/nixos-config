(use-package codesearch
  :bind
  (:map custom-projects-map
        ("c" . listing-codesearch-search))
  :custom
  (codesearch-global-csearchindex "@globalWorkspaceRoot@/.csearchindex"))

(use-package consult-codesearch
  :bind
  (:map custom-projects-map
        ("." . consult-codesearch-find-file)
        ("s" . consult-codesearch)
        ("I" . consult-codesearch-build-index)))
