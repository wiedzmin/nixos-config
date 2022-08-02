{ config, inputs, lib, pkgs, ... }:

let
  nixpkgs-mspyls = import inputs.nixpkgs-mspyls {
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  };
in
rec {
  lspPythonMsExecutable = "${nixpkgs-mspyls.python-language-server}/bin/python-language-server";
  lspPythonMsExtraPaths = builtins.concatStringsSep " " (lib.forEach config.dev.python.pylsExtraSourcePaths (path: ''"${path}"''));
}
