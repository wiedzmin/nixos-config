{ config, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  emacsVueTsModePath = inputs.emacs-vue-ts-mode;
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModeCssRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "css-mode-hook" ] [ "vscode-css-language-server" "--stdio" ] "css" "css-ls"
    else "";
  lspModeCssTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "css-ts-mode-hook" ] [ "vscode-css-language-server" "--stdio" ] "css" "css-ls"
    else "";
  lspModeHtmlRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "html-mode-hook" ] [ "vscode-html-language-server" "--stdio" ] "html" "html-ls"
    else "";
  lspModeHtmlTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "html-ts-mode-hook" ] [ "vscode-html-language-server" "--stdio" ] "html" "html-ls"
    else "";
  lspModeJsRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "js-mode-hook" ] [ "typescript-language-server" "--stdio" ] "javascript" "js-ls"
    else "";
  lspModeJsTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "js-ts-mode-hook" ] [ "typescript-language-server" "--stdio" ] "javascript" "js-ls"
    else "";
  lspModeJsonRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "js-json-mode-hook" ] [ "typescript-language-server" "--stdio" ] "json" "json-ls"
    else "";
  lspModeJsonTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "json-ts-mode-hook" ] [ "typescript-language-server" "--stdio" ] "json" "json-ls"
    else "";
  lspModeVueRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "vue-mode-hook" ] [ "vls" "--stdio" ] "vue" "vue-ls"
    else "";
  lspModeVueTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "vue-ts-mode-hook" ] [ "vls" "--stdio" ] "vue" "vue-ls"
    else "";
  eglotVueRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "vue-mode-hook" ] [ "vls" "--stdio" ] [ "vue-mode" ]
    else "";
  eglotVueTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "vue-ts-mode-hook" ] [ "vls" "--stdio" ] [ "vue-ts-mode" ]
    else "";
}
