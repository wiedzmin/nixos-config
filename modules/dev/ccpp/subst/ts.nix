{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModeCTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "c-ts-mode-hook" ] [ "ccls" ] "c" "c-lsp"
    else "";
  lspModeCPPTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "c++-ts-mode-hook" ] [ "ccls" ] "cpp" "cpp-lsp"
    else "";
  eglotCTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "c-ts-mode-hook" ] [ "ccls" ] [ "c-ts-mode" ]
    else "";
  eglotCPPTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "c++-ts-mode-hook" ] [ "ccls" ] [ "c++-ts-mode" ]
    else "";
  lspModeCmakeTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "cmake-ts-mode-hook" ] [ "neocmakelsp" "--stdio" ] "cmake" "cmake-lsp"
    else "";
  eglotCmakeTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "cmake-ts-mode-hook" ] [ "neocmakelsp" "--stdio" ] [ "cmake-ts-mode" ]
    else "";
}
