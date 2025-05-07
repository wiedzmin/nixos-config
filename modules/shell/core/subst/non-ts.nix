{ config, pkgs, ... }:

with pkgs.unstable.commonutils;

{
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModeShellRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "sh-mode-hook" ] [ "bash-language-server" "start" ] "shellscript" "bash-lsp"
    else "";
}
