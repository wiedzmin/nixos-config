{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  lspStartFunctionJson = config.dev.misc.emacs.lsp.startFunction;
  lspModeJsonRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "js-json-mode-hook" ] [ "typescript-language-server" "--stdio" ] "json" "json-ls"
    else "";
}
