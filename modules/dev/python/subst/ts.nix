{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModePythonTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "python-ts-mode-hook" ] [ "pylsp" ] "python" "pylsp"
    else "";
}
