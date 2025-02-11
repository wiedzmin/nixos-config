(use-package blockdiag-mode)

(use-package graphviz-dot-mode
  :hook (graphviz-dot-mode-hook . @lspStartFunctionGraphviz@)
  @lspModeGraphvizRegisterServer@@eglotGraphvizRegisterServer@)

(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path "@plantumlJar@")
  (org-plantuml-jar-path plantuml-jar-path)
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))
