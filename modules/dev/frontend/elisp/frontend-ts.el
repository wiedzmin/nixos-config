(use-package css-ts-mode
  :mode "\\.css$"
  :hook (css-ts-mode-hook . @lspStartFunction@)
  @lspModeCssTSRegisterServer@)

(use-package html-ts-mode
  :mode "\\.html$"
  :hook (html-ts-mode-hook . @lspStartFunction@)
  @lspModeHtmlTSRegisterServer@)

(use-package js-ts-mode
  :mode "\\.js$"
  :hook
  (js-ts-mode-hook . @lspStartFunction@)
  @lspModeJsTSRegisterServer@)

(use-package json-ts-mode
  :mode "\\.json$"
  :hook
  (json-ts-mode-hook . @lspStartFunction@)
  @lspModeJsonTSRegisterServer@)

(use-package vue-ts-mode
  :load-path "@emacsVueTsModePath@"
  :mode "\\.vue$"
  :hook (vue-ts-mode-hook . @lspStartFunction@)
  @lspModeVueTSRegisterServer@@eglotVueTSRegisterServer@)
