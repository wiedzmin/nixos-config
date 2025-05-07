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
  eglotCRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "c-mode-hook" ] [ "ccls" ] [ "c-mode" ]
    else "";
  eglotCPPRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "c++-mode-hook" ] [ "ccls" ] [ "c++-mode" ]
    else "";
  lspModeCmakeRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "cmake-mode-hook" ] [ "neocmakelsp" "--stdio" ] "cmake" "cmake-lsp"
    else "";
  eglotCmakeRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "cmake-mode-hook" ] [ "neocmakelsp" "--stdio" ] [ "cmake-mode" ]
    else "";
}
