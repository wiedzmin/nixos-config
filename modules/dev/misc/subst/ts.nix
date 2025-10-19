{ config, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  lspStartFunctionJson = config.dev.misc.emacs.lsp.startFunction;
  lspModeJsonTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "json-ts-mode-hook" ] [ "typescript-language-server" "--stdio" ] "json" "json-ls"
    else "";
}
