(use-package css-mode
  :mode "\\.css$"
  :hook
  (css-mode-hook . @lspStartFunction@)
  @lspModeCssRegisterServer@)

(use-package html-mode
  :mode "\\.html$"
  :hook (html-mode-hook . @lspStartFunction@)
  @lspModeHtmlRegisterServer@)

(use-package js-mode
  :mode "\\.js$"
  :hook
  (css-mode-hook . @lspStartFunction@)
  @lspModeJsRegisterServer@)

(use-package vue-mode
  :mode "\\.vue$"
  :hook (vue-mode-hook . @lspStartFunction@)
  @lspModeVueRegisterServer@@eglotVueRegisterServer@)
