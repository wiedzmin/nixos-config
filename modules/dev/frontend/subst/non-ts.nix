{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModeCssRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "css-mode-hook" ] [ "vscode-css-language-server" "--stdio" ] "css" "css-ls"
    else "";
  lspModeHtmlRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "html-mode-hook" ] [ "vscode-html-language-server" "--stdio" ] "html" "html-ls"
    else "";
  lspModeJsRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "js-mode-hook" ] [ "typescript-language-server" "--stdio" ] "javascript" "js-ls"
    else "";
  lspModeVueRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "vue-mode-hook" ] [ "vls" "--stdio" ] "vue" "vue-ls"
    else "";
  eglotVueRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "vue-mode-hook" ] [ "vls" "--stdio" ] [ "vue-mode" ]
    else "";
}
