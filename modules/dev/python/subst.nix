{ config, lib, pkgs, ... }:

rec {
  lspPythonMsExecutable = "${pkgs.python-language-server}/bin/python-language-server";
  lspPythonMsExtraPaths = builtins.concatStringsSep " " (lib.forEach config.dev.python.pylsExtraSourcePaths (path: ''"${path}"''));
}
