{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspPackageName = config.dev.misc.emacs.lsp.packageName;
}
