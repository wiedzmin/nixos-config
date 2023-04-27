{ config, lib, pkgs, ... }:

{
  lspPythonMsExecutable = "${pkgs.python3Packages.python-lsp-server}/bin/python-lsp-server";
  lspPythonMsExtraPaths = builtins.concatStringsSep " " (lib.forEach config.dev.python.pylsExtraSourcePaths (path: ''"${path}"''));
}
