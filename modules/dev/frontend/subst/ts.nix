{ config, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  emacsVueTsModePath = inputs.emacs-vue-ts-mode;
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModeCssTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "css-ts-mode-hook" ] [ "vscode-css-language-server" "--stdio" ] "css" "css-ls"
    else "";
  lspModeHtmlTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "html-ts-mode-hook" ] [ "vscode-html-language-server" "--stdio" ] "html" "html-ls"
    else "";
  lspModeJsTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "js-ts-mode-hook" ] [ "typescript-language-server" "--stdio" ] "javascript" "js-ls"
    else "";
  lspModeVueTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "vue-ts-mode-hook" ] [ "vls" "--stdio" ] "vue" "vue-ls"
    else "";
  eglotVueTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "vue-ts-mode-hook" ] [ "vls" "--stdio" ] [ "vue-ts-mode" ]
    else "";
}
