{ config, lib, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  combyExcludes = lib.concatStringsSep "," config.dev.misc.comby.excludes;
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  justBinary = "${pkgs.just}/bin/just";
  lspStartFunctionYaml = emacsMkLspStartFunction [ "yaml-mode-hook" "yaml-ts-mode-hook" ] config.dev.misc.emacs.lsp.startFunction;
  lspModeYamlRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "yaml-mode-hook" "yaml-ts-mode-hook" ] [ "yaml-language-server" "--stdio" ] "yaml" "yaml-lsp"
    else "";
  lspStartFunctionGraphviz = config.dev.misc.emacs.lsp.startFunction;
  lspModeGraphvizRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "graphviz-dot-mode-hook" ] [ "dot-language-server" "--stdio" ] "dot" "dotls"
    else "";
  eglotGraphvizRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "graphviz-dot-mode-hook" ] [ "dot-language-server" "--stdio" ] [ "graphviz-dot-mode" ]
    else "";
  lspStartFunctionJson = config.dev.misc.emacs.lsp.startFunction;
  lspModeJsonRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "js-json-mode-hook" ] [ "typescript-language-server" "--stdio" ] "json" "json-ls"
    else "";
  lspModeJsonTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "json-ts-mode-hook" ] [ "typescript-language-server" "--stdio" ] "json" "json-ls"
    else "";
}
