{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModeDockerfileRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "dockerfile-mode-hook" ] [ "docker-langserver" "--stdio" ] "dockerfile" "docker-lsp"
    else "";
  lspModeDockerfileTSRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "dockerfile-ts-mode-hook" ] [ "docker-langserver" "--stdio" ] "dockerfile" "docker-lsp"
    else "";
}
