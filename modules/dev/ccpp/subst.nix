{ pkgs, ... }:

rec {
  cclsExecutable = "${pkgs.ccls}/bin/ccls";
}
