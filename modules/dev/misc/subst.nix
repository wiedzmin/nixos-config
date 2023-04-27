{ config, lib, pkgs, ... }:

{
  combyExcludes = lib.concatStringsSep "," config.dev.misc.comby.excludes;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  justBinary = "${pkgs.just}/bin/just";
}
