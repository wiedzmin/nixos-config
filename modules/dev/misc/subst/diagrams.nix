{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  lspStartFunctionGraphviz = config.dev.misc.emacs.lsp.startFunction;
  lspModeGraphvizRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "graphviz-dot-mode-hook" ] [ "dot-language-server" "--stdio" ] "dot" "dotls"
    else "";
  eglotGraphvizRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "graphviz-dot-mode-hook" ] [ "dot-language-server" "--stdio" ] [ "graphviz-dot-mode" ]
    else "";
}
