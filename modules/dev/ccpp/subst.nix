{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  cclsExecutable = "${pkgs.ccls}/bin/ccls";
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspPackageName = config.dev.misc.emacs.lsp.packageName;
  lspModeCRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "c-mode-hook" ] [ "ccls" ] "c" "c-lsp"
    else "";
  lspModeCPPRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "c++-mode-hook" ] [ "ccls" ] "cpp" "cpp-lsp"
    else "";
  lspModeCTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "c-ts-mode-hook" ] [ "ccls" ] "c" "c-lsp"
    else "";
  lspModeCPPTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "c++-ts-mode-hook" ] [ "ccls" ] "cpp" "cpp-lsp"
    else "";
  eglotCRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "c-mode-hook" ] [ "ccls" ] [ "c-mode" ]
    else "";
  eglotCPPRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "c++-mode-hook" ] [ "ccls" ] [ "c++-mode" ]
    else "";
  eglotCTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "c-ts-mode-hook" ] [ "ccls" ] [ "c-ts-mode" ]
    else "";
  eglotCPPTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "c++-ts-mode-hook" ] [ "ccls" ] [ "c++-ts-mode" ]
    else "";
}
