(use-package graphviz-dot-mode
  :hook (graphviz-dot-mode-hook . @lspStartFunctionGraphviz@)
  @lspModeGraphvizRegisterServer@@eglotGraphvizRegisterServer@)
