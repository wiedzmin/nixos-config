{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  lspModeYamlRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "yaml-mode-hook" "yaml-ts-mode-hook" ] [ "yaml-language-server" "--stdio" ] "yaml" "yaml-lsp"
    else "";
  lspStartFunctionYaml = emacsMkLspStartFunction [ "yaml-mode-hook" "yaml-ts-mode-hook" ] config.dev.misc.emacs.lsp.startFunction;
  justBinary = "${pkgs.just}/bin/just";
  combyExcludes = lib.concatStringsSep "," config.dev.misc.comby.excludes;
}
