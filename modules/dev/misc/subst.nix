{ config, lib, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  combyExcludes = lib.concatStringsSep "," config.dev.misc.comby.excludes;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  justBinary = "${pkgs.just}/bin/just";
  lspStartFunction = emacsMkLspStartFunction [ "yaml-mode-hook" "yaml-ts-mode-hook" ] config.dev.misc.emacs.lsp.startFunction;
  lspModeYamlRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "yaml-mode-hook" "yaml-ts-mode-hook" ] [ "yaml-language-server" "--stdio" ] "yaml" "yaml-lsp"
    else "";
  emacsJustTsModePath = inputs.just-ts-mode;
}
