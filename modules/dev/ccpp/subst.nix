{ config, inputs, lib, pkgs, ... }:

rec {
  cclsExecutable = "${pkgs.ccls}/bin/ccls";
}
