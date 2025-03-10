(use-package just-mode
  :mode
  ("\\justfile\\'" . just-mode))

(use-package lua-mode
  :mode
  ("\\lua\\'" . lua-mode))

(use-package js-json-mode
  :mode "\\.json$"
  :hook
  (js-json-mode-hook . @lspStartFunctionJson@)
  @lspModeJsonRegisterServer@)
