{ config, pkgs, ... }:

with pkgs.unstable.commonutils;

{
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModeShellTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "bash-ts-mode-hook" ] [ "bash-language-server" "start" ] "shellscript" "bash-lsp"
    else "";
}
