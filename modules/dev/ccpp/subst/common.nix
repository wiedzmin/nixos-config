{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  cclsExecutable = "${pkgs.ccls}/bin/ccls";
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspPackageName = config.dev.misc.emacs.lsp.packageName;
}
