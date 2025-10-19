(use-package just-ts-mode
  :mode
  ("\\justfile\\'" . just-ts-mode))

(use-package lua-ts-mode
  :mode
  ("\\lua\\'" . lua-ts-mode))

(use-package json-ts-mode
  :mode "\\.json$"
  :hook
  (json-ts-mode-hook . @lspStartFunctionJson@)
  @lspModeJsonTSRegisterServer@)
