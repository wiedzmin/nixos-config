{ config, lib, pkgs, ... }:

rec {
  combyExcludes = lib.concatStringsSep "," config.dev.misc.comby.excludes;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
}
