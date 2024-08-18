{ config, pkgs, ... }:

with pkgs.unstable.commonutils;

{
  lspPackageName = config.dev.misc.emacs.lsp.packageName;
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModeShellRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "sh-mode-hook" ] [ "bash-language-server" "start" ] "shellscript" "bash-lsp"
    else "";
  lspModeShellTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "bash-ts-mode-hook" ] [ "bash-language-server" "start" ] "shellscript" "bash-lsp"
    else "";
}
